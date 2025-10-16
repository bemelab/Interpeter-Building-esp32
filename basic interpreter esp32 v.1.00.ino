#include <Arduino.h>
#include <math.h>

// --- Configuration ---
#define STACK_SIZE 100
#define MAX_PROGRAM_LINES 250
#define MAX_LOOP_DEPTH 8


// --- Constants ---
#define PI_CONST 3.141592653589793
#define E_CONST  2.718281828459045

// =================================================================
// Global Variables for BASIC Interpreter
// =================================================================
double variableValues[26];
String programLines[MAX_PROGRAM_LINES];
int    lineNumbers[MAX_PROGRAM_LINES];
int    programSize = 0;
enum ProgramState { IDLE, RUNNING, AWAIT_INPUT };
ProgramState programState = IDLE;
int pc = 0;
int awaitingInputForVarIndex = -1;
String inputBuffer = "";
int printPrecision = 15;
double* arrayData[26];
int arraySizes[26];

struct ForLoopState {
  int variableIndex;
  double endValue;
  double stepValue;
  int returnAddress;
};
ForLoopState forLoopStack[MAX_LOOP_DEPTH];
int forLoopStackPtr = -1;

// --- Forward Declarations & Helper Functions ---
void newProgram(); void listProgram(); void startProgram(); void executeNextBasicLine(); void processInput(String input); void executeImmediate(String line); void breakProgram();
bool isVariable(char c) {
  char uc = toupper(c);
  return uc >= 'A' && uc <= 'Z';
}
int getVariableIndex(char c) {
  if (!isVariable(c)) return -1;
  return toupper(c) - 'A';
}
bool isProgramLineStart(String s) {
  // A program line must start with an integer line number (no decimal point),
  // optionally followed by a space and code. Expressions like 0.5^0.5 must NOT match.
  s.trim();
  if (s.length() == 0) return false;
  int i = 0;
  // Allow leading spaces already trimmed; read digits
  while (i < s.length() && isDigit(s.charAt(i))) {
    i++;
  }
  if (i == 0) return false; // no leading digits
  // If next char is a '.', it's a decimal, not a line number
  if (i < s.length() && s.charAt(i) == '.') return false;
  // If we consumed all chars, it's just a number; treat as program line number
  if (i == s.length()) return true;
  // Otherwise require a separating space before code
  return s.charAt(i) == ' ';
}
int roundToInt(double v) {
  return (int)(v + (v >= 0 ? 0.5 : -0.5));
}
String removeSpaces(String s) {
  String out = "";
  out.reserve(s.length());
  for (int i = 0; i < s.length(); i++) {
    char c = s.charAt(i);
    if (c != ' ') out += c;
  }
  return out;
}
bool isArrayAllocated(int varIdx) {
  return varIdx >= 0 && varIdx < 26 && arrayData[varIdx] != NULL && arraySizes[varIdx] > 0;
}
double arrayGetValue(int varIdx, int index) {
  if (!isArrayAllocated(varIdx)) {
    Serial.println("Array not DIMmed!");
    return NAN;
  }
  if (index < 0 || index >= arraySizes[varIdx]) {
    Serial.println("Array index out of range!");
    return NAN;
  }
  return arrayData[varIdx][index];
}
void arraySetValue(int varIdx, int index, double value) {
  if (index < 0) {
    Serial.println("Array index out of range!");
    return;
  }
  // Auto-allocate if not DIMmed yet
  if (!isArrayAllocated(varIdx)) {
    int n = index + 1;
    if (n > 1024) n = 1024;
    if (n <= 0) n = 0;
    if (n > 0) {
      arrayData[varIdx] = new double[n];
      for (int i = 0; i < n; i++) arrayData[varIdx][i] = 0.0;
      arraySizes[varIdx] = n;
    } else {
      Serial.println("Array not DIMmed!");
      return;
    }
  }
  // Grow if needed (up to cap)
  if (index >= arraySizes[varIdx]) {
    int newSize = index + 1;
    if (newSize > 1024) {
      Serial.println("Array index out of range!");
      return;
    }
    double* newArr = new double[newSize];
    for (int i = 0; i < newSize; i++) newArr[i] = 0.0;
    for (int i = 0; i < arraySizes[varIdx]; i++) newArr[i] = arrayData[varIdx][i];
    delete[] arrayData[varIdx];
    arrayData[varIdx] = newArr;
    arraySizes[varIdx] = newSize;
  }
  arrayData[varIdx][index] = value;
}


// =================================================================
// Stacks & Calculator Class
// =================================================================
class CharStack {
  public: CharStack(int s = STACK_SIZE) {
      maxSize = s;
      stackArray = new String[maxSize];
      top = -1;
    }~CharStack() {
      delete[] stackArray;
    } void push(String v) {
      if (!isFull())stackArray[++top] = v;
    } String pop() {
      return!isEmpty() ? stackArray[top--] : "";
    } String peek() {
      return!isEmpty() ? stackArray[top] : "";
    } bool isEmpty() {
      return top == -1;
    } bool isFull() {
      return top == maxSize - 1;
    } private: String*stackArray;
    int top, maxSize;
};
class FloatStack {
  public: FloatStack(int s = STACK_SIZE) {
      maxSize = s;
      stackArray = new double[maxSize];
      top = -1;
    }~FloatStack() {
      delete[] stackArray;
    } void push(double v) {
      if (!isFull())stackArray[++top] = v;
    } double pop() {
      return!isEmpty() ? stackArray[top--] : NAN;
    } private: double*stackArray;
    int top, maxSize;
    bool isEmpty() {
      return top == -1;
    } bool isFull() {
      return top == maxSize - 1;
    }
};
class Calculator {
  public: String toPostfix(String i);
    double evaluatePostfix(String p, bool isRad);
  private: int getPrecedence(String o);
    bool isFunc(String t);
    String handleUnary(String i);
    double power(double b, double e);
};
String Calculator::handleUnary(String i) {
  i.replace(" ", "");
  i.replace("(+", "(");
  i.replace("*+", "*");
  i.replace("/+", "/");
  i.replace("++", "+");
  i.replace("-+", "-");
  i.replace("%+", "%");
  i.replace("^+", "^");
  if (i.startsWith("+"))i = i.substring(1);
  if (i.startsWith("-"))i = "0" + i;
  i.replace("(-", "(0-");
  return i;
}
int Calculator::getPrecedence(String o) {
  if (isFunc(o))return 4;
  if (o == "^")return 3;
  if (o == "*" || o == "/" || o == "%")return 2;
  if (o == "+" || o == "-")return 1;
  return 0;
}
bool Calculator::isFunc(String t) {
  const char*f[] = {"SIN", "COS", "TAN", "ASIN", "ACOS", "ATAN", "SIND", "COSD", "TAND", "SINH", "COSH", "TANH", "ASINH", "ACOSH", "ATANH", "LN", "LOG", "EXP", "SQRT", "RAD", "DEG"};
  for (const char*i : f) {
    if (t.equalsIgnoreCase(i))return true;
  }
  // Treat array getters as functions: GET_A, GET_B, ...
  if (t.length() > 4 && t.substring(0, 4).equalsIgnoreCase("GET_")) return true;
  return false;
}

String Calculator::toPostfix(String i) {
  i.replace("π", "pi");
  i = handleUnary(i);
  String p = "";
  CharStack o;
  bool prevWasValueOrRightParen = false;
  for (int k = 0; k < i.length(); ) {
    char c = i.charAt(k);
    if (isDigit(c) || c == '.') {
      String n = "";
      while (k < i.length() && (isDigit(i.charAt(k)) || i.charAt(k) == '.')) n += i.charAt(k++);
      p += n + " ";
      prevWasValueOrRightParen = true;
      continue;
    }
    if (isAlpha(c)) {
      String token = "";
      while (k < i.length() && isAlpha(i.charAt(k))) token += i.charAt(k++);

      String upperToken = token;
      upperToken.toUpperCase();

      if (isFunc(upperToken)) {
        o.push(upperToken);
      }
      else if (upperToken == "PI") {
        p += "pi ";
      }
      else if (upperToken == "E") {
        p += "e ";
      }
      else if (upperToken.length() == 1 && isVariable(upperToken.charAt(0))) {
        // Check for array access like A(...)
        if (k < i.length() && i.charAt(k) == '(') {
          // Push function-like GET_A and an opening parenthesis to parse index expression
          String getTok = String("GET_") + upperToken;
          o.push(getTok);
          o.push("(");
          k++; // consume '('
          // Do not emit the variable itself; value will be provided by GET_A after ')'
        } else {
          p += upperToken + " ";
          prevWasValueOrRightParen = true;
        }
      }
      prevWasValueOrRightParen = true;
      continue;
    }
    if (c == '(') {
      o.push("(");
      k++;
      prevWasValueOrRightParen = false;
      continue;
    }
    if (c == ')') {
      while (!o.isEmpty() && o.peek() != "(") p += o.pop() + " ";
      o.pop();
      if (!o.isEmpty() && isFunc(o.peek())) p += o.pop() + " ";
      k++;
      prevWasValueOrRightParen = true;
      continue;
    }
    if (String("+-*/%^").indexOf(c) != -1) {
      String o1 = String(c);
      if (!prevWasValueOrRightParen && (c == '-' || c == '+')) {
        // Treat leading '+'/'-' as unary by injecting a 0 before the operator
        p += "0 ";
      }
      while (!o.isEmpty() && o.peek() != "(" && ((getPrecedence(o1) < getPrecedence(o.peek())) || (getPrecedence(o1) == getPrecedence(o.peek()) && o1 != "^"))) p += o.pop() + " ";
      o.push(o1);
      k++;
      prevWasValueOrRightParen = false;
      continue;
    }
    k++;
  }
  while (!o.isEmpty()) p += o.pop() + " ";
  p.trim();
  return p;
}

double Calculator::evaluatePostfix(String p, bool r) {
  FloatStack v;
  int c = 0;
  while (c < p.length()) {
    int n = p.indexOf(' ', c);
    if (n == -1)n = p.length();
    String t = p.substring(c, n);
    c = n + 1;
    if (t.length() == 0)continue;
    if (
      isDigit(t.charAt(0)) ||
      t.charAt(0) == '.' ||
      (t.charAt(0) == '-' && t.length() > 1 && (isDigit(t.charAt(1)) || t.charAt(1) == '.'))
    ) v.push(t.toDouble());
    else if (t.equalsIgnoreCase("pi"))v.push(PI_CONST);
    else if (t.equalsIgnoreCase("e"))v.push(E_CONST);
    else if (t.length() == 1 && isVariable(t.charAt(0)))v.push(variableValues[getVariableIndex(t.charAt(0))]);
    else if (isFunc(t)) {
      double val = v.pop(), res = 0;
      String ut = t;
      ut.toUpperCase();
      if (ut == "SIN")res = r ? sin(val) : sin(val * PI_CONST / 180.0);
      else if (ut == "COS")res = r ? cos(val) : cos(val * PI_CONST / 180.0);
      else if (ut == "TAN")res = r ? tan(val) : tan(val * PI_CONST / 180.0);
      else if (ut == "ASIN")res = r ? asin(val) : asin(val) * 180.0 / PI_CONST;
      else if (ut == "ACOS")res = r ? acos(val) : acos(val) * 180.0 / PI_CONST;
      else if (ut == "ATAN")res = r ? atan(val) : atan(val) * 180.0 / PI_CONST;
      else if (ut == "SIND")res = sin(val * PI_CONST / 180.0);
      else if (ut == "COSD")res = cos(val * PI_CONST / 180.0);
      else if (ut == "TAND")res = tan(val * PI_CONST / 180.0);
      else if (ut == "SINH")res = sinh(val);
      else if (ut == "COSH")res = cosh(val);
      else if (ut == "TANH")res = tanh(val);
      else if (ut == "ASINH")res = asinh(val);
      else if (ut == "ACOSH")res = acosh(val);
      else if (ut == "ATANH")res = atanh(val);
      else if (ut == "LN")res = log(val);
      else if (ut == "LOG")res = log10(val);
      else if (ut == "EXP")res = exp(val);
      else if (ut == "SQRT")res = sqrt(val);
      else if (ut == "RAD") res = val * PI_CONST / 180.0;
      else if (ut == "DEG") res = val * 180.0 / PI_CONST;
      else if (ut.length() == 5 && ut.substring(0,4) == String("GET_")) {
        // Array get: token like GET_A
        char arrName = ut.charAt(4);
        int varIdx = getVariableIndex(arrName);
        int idx = roundToInt(val);
        res = arrayGetValue(varIdx, idx);
      }
      v.push(res);
    } else {
      double v2 = v.pop(), v1 = v.pop();
      if (t == "+")v.push(v1 + v2);
      else if (t == "-")v.push(v1 - v2);
      else if (t == "*")v.push(v1 * v2);
      else if (t == "/")v.push((v2 == 0.0) ? NAN : (v1 / v2));
      else if (t == "%")v.push((v2 == 0.0) ? NAN : fmod(v1, v2));
      else if (t == "^")v.push(power(v1, v2));
    }
  } return v.pop();
}
double Calculator::power(double b, double e) {
  if (e == 0)return 1;
  if (b == 0) {
    if (e > 0) return 0;
    if (e == 0) return 1;
    return NAN; // 0 raised to negative exponent is undefined
  }
  if (floor(e) == e && e > 0) {
    long i_e = (long)e;
    double r = 1.0;
    while (i_e > 0) {
      if (i_e % 2 == 1)r *= b;
      b *= b;
      i_e /= 2;
    } return r;
  } return pow(b, e);
}

Calculator calculator;
bool isRadiansMode = true;

// =================================================================
// BASIC Interpreter Implementation
// =================================================================
int findLineIndex(int lineNum) {
  for (int i = 0; i < programSize; i++) {
    if (lineNumbers[i] == lineNum)return i;
  } return -1;
}
void storeLine(String line) {
  line.trim();
  int spacePos = line.indexOf(' ');
  if (spacePos == -1 && line.toInt() > 0) {
    spacePos = line.length();
  } else if (spacePos == -1) {
    return;
  } int lineNum = line.substring(0, spacePos).toInt();
  if (lineNum == 0)return;
  String code = line.substring(spacePos);
  code.trim();
  int existingIndex = findLineIndex(lineNum);
  if (existingIndex != -1) {
    if (code.length() == 0) {
      programSize--;
      for (int i = existingIndex; i < programSize; i++) {
        lineNumbers[i] = lineNumbers[i + 1];
        programLines[i] = programLines[i + 1];
      }
    } else {
      programLines[existingIndex] = code;
    }
  } else {
    if (code.length() == 0)return;
    if (programSize >= MAX_PROGRAM_LINES) {
      Serial.println("Program memory full!");
      return;
    } int insertPos = 0;
    while (insertPos < programSize && lineNumbers[insertPos] < lineNum)insertPos++;
    for (int i = programSize; i > insertPos; i--) {
      lineNumbers[i] = lineNumbers[i - 1];
      programLines[i] = programLines[i - 1];
    } lineNumbers[insertPos] = lineNum;
    programLines[insertPos] = code;
    programSize++;
  }
}

void newProgram() {
  programSize = 0;
  for (int i = 0; i < 26; i++)variableValues[i] = 0;
  programState = IDLE;
  forLoopStackPtr = -1;
  // Free arrays on NEW
  for (int i = 0; i < 26; i++) {
    if (arrayData[i] != NULL) {
      delete[] arrayData[i];
      arrayData[i] = NULL;
    }
    arraySizes[i] = 0;
  }
  Serial.println("OK");
}
void listProgram() {
  for (int i = 0; i < programSize; i++) {
    Serial.print(lineNumbers[i]);
    Serial.print(" ");
    Serial.println(programLines[i]);
  }
}
void startProgram() {
  if (programSize > 0) {
    pc = 0;
    programState = RUNNING;
    forLoopStackPtr = -1;
  }
}
void executeNextBasicLine() {
  if (pc >= programSize) {
    programState = IDLE;
    return;
  } String line = programLines[pc];
  String command, args;
  int firstSpace = line.indexOf(' ');
  if (firstSpace != -1) {
    command = line.substring(0, firstSpace);
    args = line.substring(firstSpace + 1);
  } else {
    command = line;
    args = "";
  } command.toUpperCase();
  args.trim();
  bool jumped = false;
  if (command == "REM") {} 
  else if (command == "RAD") {
    isRadiansMode = true;
  }
  else if (command == "DEG") {
    isRadiansMode = false;
  }
  else if (command == "END") {
    programState = IDLE;
  } else if (command == "PRINT") {
    while (args.length() > 0) {
      int semiPos = args.indexOf(';');
      String token;
      bool trailingSemicolon = false;
      if (semiPos != -1) {
        token = args.substring(0, semiPos);
        args = args.substring(semiPos + 1);
        trailingSemicolon = true;
      } else {
        token = args;
        args = "";
      }
      token.trim();
      if (token.startsWith("\"") && token.endsWith("\"")) {
        Serial.print(token.substring(1, token.length() - 1));
      } else if (token.length() > 0) {
        Serial.print(calculator.evaluatePostfix(calculator.toPostfix(token), isRadiansMode), printPrecision);
      }
      if (!trailingSemicolon) {
        Serial.println();
      }
    }
  } else if (command == "INPUT") {
    awaitingInputForVarIndex = getVariableIndex(args.charAt(0));
    if (awaitingInputForVarIndex != -1) {
      programState = AWAIT_INPUT;
      Serial.print("? ");
    }
  } else if (command == "GOTO") {
    int newPc = findLineIndex(args.toInt());
    if (newPc != -1) {
      pc = newPc;
      jumped = true;
    }
  } else if (command == "PREC") {
    int val = args.toInt();
    if (val < 0) val = 0;
    if (val > 15) val = 15;
    printPrecision = val;
  } else if (command == "DIM") {
    // Syntax: DIM A(n)
    args.trim();
    if (args.length() >= 4 && isVariable(args.charAt(0))) {
      int varIdx = getVariableIndex(args.charAt(0));
      int lp = args.indexOf('(');
      int rp = args.lastIndexOf(')');
      if (lp != -1 && rp != -1 && rp > lp + 1) {
        String nExpr = args.substring(lp + 1, rp);
        int n = roundToInt(calculator.evaluatePostfix(calculator.toPostfix(nExpr), isRadiansMode));
        if (n < 0) n = 0;
        if (n > 1024) n = 1024; // hard limit
        if (arrayData[varIdx] != NULL) {
          delete[] arrayData[varIdx];
          arrayData[varIdx] = NULL;
          arraySizes[varIdx] = 0;
        }
        if (n > 0) {
          arrayData[varIdx] = new double[n];
          for (int i = 0; i < n; i++) arrayData[varIdx][i] = 0.0;
          arraySizes[varIdx] = n;
        }
      }
    }
  } else if (command == "IF") {
    String uArgs = args;
    uArgs.toUpperCase();
    int thenPos = uArgs.indexOf("THEN");
    if (thenPos != -1) {
      String cStr = args.substring(0, thenPos);
      int targetLine = args.substring(thenPos + 4).toInt();
      String op;
      int opPos = -1;
      if ((opPos = cStr.indexOf("<>")) != -1)op = "<>";
      else if ((opPos = cStr.indexOf("!=")) != -1)op = "!=";
      else if ((opPos = cStr.indexOf("<=")) != -1)op = "<=";
      else if ((opPos = cStr.indexOf(">=")) != -1)op = ">=";
      else if ((opPos = cStr.indexOf('<')) != -1)op = "<";
      else if ((opPos = cStr.indexOf('>')) != -1)op = ">";
      else if ((opPos = cStr.indexOf('=')) != -1)op = "=";
      if (opPos != -1) {
        double lhs = calculator.evaluatePostfix(calculator.toPostfix(cStr.substring(0, opPos)), isRadiansMode);
        double rhs = calculator.evaluatePostfix(calculator.toPostfix(cStr.substring(opPos + op.length())), isRadiansMode);
        bool conditionMet = false;
        if (op == "<")conditionMet = lhs < rhs;
        else if (op == ">")conditionMet = lhs > rhs;
        else if (op == "<=")conditionMet = lhs <= rhs;
        else if (op == ">=")conditionMet = lhs >= rhs;
        else if (op == "=")conditionMet = lhs == rhs;
        else if (op == "<>" || op == "!=")conditionMet = lhs != rhs;
        if (conditionMet) {
          int newPc = findLineIndex(targetLine);
          if (newPc != -1) {
            pc = newPc;
            jumped = true;
          }
        }
      }
    }
  } else if (command == "FOR") {
    String uArgs = args;
    uArgs.toUpperCase();
    int eqPos = uArgs.indexOf('=');
    int toPos = uArgs.indexOf("TO");
    int stepPos = uArgs.indexOf("STEP");
    if (eqPos != -1 && toPos != -1) {
      int varIdx = getVariableIndex(uArgs.charAt(0));
      String startExpr = args.substring(eqPos + 1, toPos);
      String endExpr, stepExpr;
      if (stepPos != -1) {
        endExpr = args.substring(toPos + 2, stepPos);
        stepExpr = args.substring(stepPos + 4);
      } else {
        endExpr = args.substring(toPos + 2);
        stepExpr = "1";
      } if (forLoopStackPtr >= MAX_LOOP_DEPTH - 1) {
        Serial.println("Loop depth exceeded!");
        programState = IDLE;
      } else {
        forLoopStackPtr++;
        forLoopStack[forLoopStackPtr].variableIndex = varIdx;
        forLoopStack[forLoopStackPtr].endValue = calculator.evaluatePostfix(calculator.toPostfix(endExpr), isRadiansMode);
        forLoopStack[forLoopStackPtr].stepValue = calculator.evaluatePostfix(calculator.toPostfix(stepExpr), isRadiansMode);
        forLoopStack[forLoopStackPtr].returnAddress = pc;
        variableValues[varIdx] = calculator.evaluatePostfix(calculator.toPostfix(startExpr), isRadiansMode);
      }
    }
  } else if (command == "NEXT") {
    int varIdx = getVariableIndex(args.charAt(0));
    if (forLoopStackPtr < 0 || forLoopStack[forLoopStackPtr].variableIndex != varIdx) {
      Serial.println("NEXT without FOR error!");
      programState = IDLE;
    } else {
      variableValues[varIdx] += forLoopStack[forLoopStackPtr].stepValue;
      bool loopContinues = (forLoopStack[forLoopStackPtr].stepValue >= 0) ? (variableValues[varIdx] <= forLoopStack[forLoopStackPtr].endValue + 0.0001) : (variableValues[varIdx] >= forLoopStack[forLoopStackPtr].endValue - 0.0001);
      if (loopContinues) {
        pc = forLoopStack[forLoopStackPtr].returnAddress + 1;
        jumped = true;
      } else {
        forLoopStackPtr--;
      }
    }
  } else {
    String uLine = line;
    uLine.toUpperCase();
    if (uLine.startsWith("LET ")) {
      line = line.substring(4);
    } int eqPos = line.indexOf('=');
    if (eqPos != -1) {
      // Array assignment? like A(5)=expr
      String lhs = line.substring(0, eqPos);
      lhs.trim();
      String rhs = line.substring(eqPos + 1);
      rhs.trim();
      if (lhs.length() >= 4 && isVariable(lhs.charAt(0)) && lhs.indexOf('(') != -1 && lhs.endsWith(")")) {
        int varIdx = getVariableIndex(lhs.charAt(0));
        int lp = lhs.indexOf('(');
        int rp = lhs.lastIndexOf(')');
        if (lp != -1 && rp != -1 && rp > lp + 1) {
          String idxExpr = lhs.substring(lp + 1, rp);
          int index = roundToInt(calculator.evaluatePostfix(calculator.toPostfix(idxExpr), isRadiansMode));
          double value = calculator.evaluatePostfix(calculator.toPostfix(rhs), isRadiansMode);
          arraySetValue(varIdx, index, value);
        }
      } else {
        int varIdx = getVariableIndex(line.charAt(0));
        if (varIdx != -1) {
          variableValues[varIdx] = calculator.evaluatePostfix(calculator.toPostfix(line.substring(eqPos + 1)), isRadiansMode);
        }
      }
    }
  } if (!jumped && programState == RUNNING) {
    pc++;
  }
}

// =================================================================
// Main Setup and Loop
// =================================================================
void setup() {
  Serial.begin(9600);
  while (!Serial) {;} newProgram();
  Serial.println("\n--- Arduino BASIC Scientific Calculator ---");
  // MODIFIED: Added instruction for the backslash delimiter
  Serial.println("Commands: RUN, LIST, NEW, BREAK, LET, PRINT, INPUT, GOTO, IF/THEN, FOR/NEXT, REM, END, RAD, DEG");
  Serial.println("NOTE: To paste code, end each line with a backslash '\\'");
  Serial.println("Extra: PREC n to set print precision (0..15)");
  Serial.println("-------------------------------------");
  isRadiansMode = true; 
  Serial.println("Mode: Radians. (Use DEG/RAD to switch)");
  Serial.println("-------------------------------------");
  Serial.print("> ");
}

void breakProgram() {
  programState = IDLE;
  forLoopStackPtr = -1;
  awaitingInputForVarIndex = -1;
  Serial.println("\n*** BREAK ***");
}

void executeImmediate(String line) {
  String safeLine = line;
  safeLine.trim();
  if (safeLine.length() == 0) return;

  String upperLine = safeLine;
  upperLine.toUpperCase();
  if (upperLine.startsWith("LET ")) {
    safeLine = safeLine.substring(4);
    safeLine.trim();
  }

  int eqPos = safeLine.indexOf('=');
  if (eqPos > 0 && isVariable(safeLine.charAt(0))) {
    // Support A(i)=expr assignments in immediate mode
    String lhs = safeLine.substring(0, eqPos);
    lhs.trim();
    String rhs = safeLine.substring(eqPos + 1);
    rhs.trim();
    if (lhs.length() >= 4 && isVariable(lhs.charAt(0)) && lhs.indexOf('(') != -1 && lhs.endsWith(")")) {
      int varIdx = getVariableIndex(lhs.charAt(0));
      int lp = lhs.indexOf('(');
      int rp = lhs.lastIndexOf(')');
      if (lp != -1 && rp != -1 && rp > lp + 1) {
        String idxExpr = lhs.substring(lp + 1, rp);
        int index = roundToInt(calculator.evaluatePostfix(calculator.toPostfix(idxExpr), isRadiansMode));
        double value = calculator.evaluatePostfix(calculator.toPostfix(rhs), isRadiansMode);
        arraySetValue(varIdx, index, value);
        Serial.print((char)('A' + varIdx));
        Serial.print("(");
        Serial.print(index);
        Serial.print(") = ");
        Serial.println(value, printPrecision);
      }
    } else {
      int varIdx = getVariableIndex(safeLine.charAt(0));
      if (varIdx != -1) {
        double res = calculator.evaluatePostfix(calculator.toPostfix(safeLine.substring(eqPos + 1)), isRadiansMode);
        variableValues[varIdx] = res;
        Serial.print((char)('A' + varIdx));
        Serial.print(" = ");
        Serial.println(res, printPrecision);
      }
    }
    return;
  }

  String command, args;
  int firstSpace = safeLine.indexOf(' ');
  if (firstSpace != -1) {
    command = safeLine.substring(0, firstSpace);
    args = safeLine.substring(firstSpace + 1);
  } else {
    command = safeLine;
    args = "";
  }

  if (command.equalsIgnoreCase("PRINT")) {
    while (args.length() > 0) {
      int semiPos = args.indexOf(';');
      String token;
      bool trailingSemicolon = false;
      if (semiPos != -1) {
        token = args.substring(0, semiPos);
        args = args.substring(semiPos + 1);
        trailingSemicolon = true;
      } else {
        token = args;
        args = "";
      }
      token.trim();
      if (token.startsWith("\"") && token.endsWith("\"")) {
        Serial.print(token.substring(1, token.length() - 1));
      } else if (token.length() > 0) {
        Serial.print(calculator.evaluatePostfix(calculator.toPostfix(token), isRadiansMode), printPrecision);
      }
      if (!trailingSemicolon) {
        Serial.println();
      }
    }
  } else if (command.equalsIgnoreCase("RUN")) {
    startProgram();
  } else if (command.equalsIgnoreCase("LIST")) {
    listProgram();
  } else if (command.equalsIgnoreCase("NEW")) {
    newProgram();
  } else if (command.equalsIgnoreCase("RAD")) {
    isRadiansMode = true;
    Serial.println("Radian Mode.");
  } else if (command.equalsIgnoreCase("DEG")) {
    isRadiansMode = false;
    Serial.println("Degree Mode.");
  } else if (command.equalsIgnoreCase("PREC")) {
    args.trim();
    int val = args.toInt();
    if (val < 0) val = 0;
    if (val > 15) val = 15;
    printPrecision = val;
    Serial.print("Precision: ");
    Serial.println(printPrecision);
  } else if (command.equalsIgnoreCase("DIM")) {
    // DIM A(n)
    args.trim();
    if (args.length() >= 4 && isVariable(args.charAt(0))) {
      int varIdx = getVariableIndex(args.charAt(0));
      int lp = args.indexOf('(');
      int rp = args.lastIndexOf(')');
      if (lp != -1 && rp != -1 && rp > lp + 1) {
        String nExpr = args.substring(lp + 1, rp);
        int n = roundToInt(calculator.evaluatePostfix(calculator.toPostfix(nExpr), isRadiansMode));
        if (n < 0) n = 0;
        if (n > 1024) n = 1024;
        if (arrayData[varIdx] != NULL) {
          delete[] arrayData[varIdx];
          arrayData[varIdx] = NULL;
          arraySizes[varIdx] = 0;
        }
        if (n > 0) {
          arrayData[varIdx] = new double[n];
          for (int i = 0; i < n; i++) arrayData[varIdx][i] = 0.0;
          arraySizes[varIdx] = n;
        }
        Serial.print("DIM ");
        Serial.print((char)('A' + varIdx));
        Serial.print("(");
        Serial.print(n);
        Serial.println(")");
      }
    }
  } else {
    Serial.println(calculator.evaluatePostfix(calculator.toPostfix(safeLine), isRadiansMode), printPrecision);
  }
}

// MODIFIED: This is the new, robust input processor using the backslash.
void processInput(String input) {
  if (input.length() == 0) return;

  // If program is awaiting input, treat the whole line as a number (or expression)
  if (programState == AWAIT_INPUT && awaitingInputForVarIndex >= 0) {
    input.trim();
    if (input.length() > 0) {
      double res = calculator.evaluatePostfix(calculator.toPostfix(input), isRadiansMode);
      variableValues[awaitingInputForVarIndex] = res;
      awaitingInputForVarIndex = -1;
      programState = RUNNING;
      pc++; // move past the INPUT line
    }
    return;
  }

  // If the input contains backslashes, split by them.
  if (input.indexOf('\\') != -1) {
    int lastPos = 0;
    while(lastPos < input.length()) {
      int slashPos = input.indexOf('\\', lastPos);
      if (slashPos == -1) {
        slashPos = input.length();
      }
      
      String segment = input.substring(lastPos, slashPos);
      segment.trim();
      if(segment.length() > 0) {
        if(isProgramLineStart(segment)) {
          storeLine(segment);
        } else {
          executeImmediate(segment);
        }
      }
      
      lastPos = slashPos + 1;
    }
  } else {
    // Otherwise, treat the whole line as a single command.
    input.trim();
    if(input.length() > 0) {
      if(isProgramLineStart(input)) {
        storeLine(input);
      } else {
        executeImmediate(input);
      }
    }
  }
}

void loop() {
  if (programState == RUNNING) {
    executeNextBasicLine();
  }

  if (Serial.available() > 0) {
    char c = Serial.read();

    if (c == '\r' || c == '\n') {
      Serial.println();
      
      String upperInput = inputBuffer;
      upperInput.toUpperCase();
      if (upperInput == "BREAK") {
        if (programState == RUNNING) {
          breakProgram();
        }
      } else {
        processInput(inputBuffer);
      }
      
      inputBuffer = "";
      
      if (programState == IDLE) {
        Serial.print("> ");
      }
    } 
    else if (c == '\b' || c == 127) {
      if (inputBuffer.length() > 0) {
        inputBuffer.remove(inputBuffer.length() - 1);
        Serial.print("\b \b");
      }
    }
    else if (isPrintable(c)) {
      inputBuffer += c;
      Serial.print(c);
    }
  }
}
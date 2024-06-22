from memory import Memory

class VirtualMachine:

    def __init__(self):
        self.codeSegment = []
        self.dirFunc = []

        self.globalSizes = ()
        self.tempSizes = ()

        self.globalMemory = Memory()
        self.tempMemory = Memory()
        self.cteMemory = Memory()

    def createMapMemory(self):
        self.globalMemory.setMemorySize(self.globalSizes)
        self.tempMemory.setMemorySize(self.tempSizes)
    
    def printDirFunc(self):
        print('DIRFUNC:')
        for func in self.dirFunc:
            print(func)

    def printCte(self):
        if self.cteMemory.memory_int:
            print("INT CONSTANTS: ")
            print(self.cteMemory.memory_int)
        if self.cteMemory.memory_float:
            print("FLOAT CONSTANTS: ")
            print(self.cteMemory.memory_float)
        if self.cteMemory.memory_bool:
            print("STRING CONSTANTS: ")
            print(self.cteMemory.memory_bool)

    def readFile(self, file):
        with open(file, 'r') as file:
            # print('DirFunc')
            for line in file:
                line = line.strip()
                
                if line.startswith('# END DIRFUNC'):
                    break
                # print(line);
                self.dirFunc.append(line)
            for line in file:
                line = line.strip()
                if line.startswith('# END QUADRUPLES'):
                    break
                self.codeSegment.append(eval(line))

            # Read the memory size needed
            for line in file:
                line = line.strip()
                
                if line.startswith('# END GLOBAL MEMORY SIZE'):
                    self.globalSizes = eval(memSizes)
                    continue

                if line.startswith('# END TEMPORAL MEMORY SIZE'):
                    self.tempSizes = eval(memSizes)
                    continue
                memSizes = line
                
            # Read the Constants Memory
                if line.startswith('# END INT CONSTANTS'):
                    self.cteMemory.memory_int = eval(cteList)
                    continue
                if line.startswith('# END FLOAT CONSTANTS'):
                    self.cteMemory.memory_float = eval(cteList)
                    continue
                if line.startswith('# END STRING CONSTANTS'):
                    self.cteMemory.memory_bool = eval(cteList)
                    continue
                cteList = line

    def getMemType(self, memoryCode):
        memType = memoryCode // 10000
        if(memType == 1):
            return self.globalMemory
        elif(memType == 2):
            return self.tempMemory
        elif(memType == 3):
            return self.cteMemory

    def getOperandsValue(self, operand1, operand2):
        # Get type of memory
        memOper1 = self.getMemType(operand1)
        memOper2 = self.getMemType(operand2)

        # Look for value in the correct memory type
        valOper1 = memOper1.get_value(operand1)
        valOper2 = memOper2.get_value(operand2)

        return valOper1, valOper2
    
    def getOperandValue(self, operand1):
         # Get type of memory
        memOper1 = self.getMemType(operand1)

        # Look for value in the correct memory type
        valOper1 = memOper1.get_value(operand1)
        return valOper1

    def runCode(self):
        self.printDirFunc()
        self.printCte()
        print('Output:')
        ip = 0
        while ip < len(self.codeSegment):
            codeOp = self.codeSegment[ip][0]
            if codeOp == 1:
                operand1, operand2 = self.codeSegment[ip][1], self.codeSegment[ip][2]
                dirResult =  self.codeSegment[ip][3]

                # Get values of operands
                valOper1, valOper2 = self.getOperandsValue(operand1, operand2)

                aux = valOper1 > valOper2
                self.tempMemory.assign_value(dirResult, aux)
                ip +=1
            elif codeOp == 2:
                operand1, operand2 = self.codeSegment[ip][1], self.codeSegment[ip][2]
                dirResult =  self.codeSegment[ip][3]

                # Get values of operands
                valOper1, valOper2 = self.getOperandsValue(operand1, operand2)

                aux = valOper1 < valOper2
                self.tempMemory.assign_value(dirResult, aux)
                ip +=1
            elif codeOp == 3:
                operand1, operand2 = self.codeSegment[ip][1], self.codeSegment[ip][2]
                dirResult =  self.codeSegment[ip][3]

                # Get values of operands
                valOper1, valOper2 = self.getOperandsValue(operand1, operand2)

                aux = valOper1 != valOper2
                self.tempMemory.assign_value(dirResult, aux)
                ip +=1
            elif codeOp == 4:
                operand1, operand2 = self.codeSegment[ip][1], self.codeSegment[ip][2]
                dirResult =  self.codeSegment[ip][3]

                # Get values of operands
                valOper1, valOper2 = self.getOperandsValue(operand1, operand2)
                
                # Perform operation
                aux = valOper1 + valOper2
                # Assign value to temporal memory
                self.tempMemory.assign_value(dirResult, aux)
                ip +=1
            elif codeOp == 5:
                # Get direction addresses
                operand1, operand2 = self.codeSegment[ip][1], self.codeSegment[ip][2]
                dirResult =  self.codeSegment[ip][3]

                # Get values of operands
                valOper1, valOper2 = self.getOperandsValue(operand1, operand2)

                aux = valOper1 - valOper2
                self.tempMemory.assign_value(dirResult, aux)
                ip +=1
            elif codeOp == 6:
                operand1, operand2 = self.codeSegment[ip][1], self.codeSegment[ip][2]
                dirResult =  self.codeSegment[ip][3]

                # Get values of operands
                valOper1, valOper2 = self.getOperandsValue(operand1, operand2)

                aux = valOper1 * valOper2
                self.tempMemory.assign_value(dirResult, aux)
                ip +=1
            elif codeOp == 7:
                operand1, operand2 = self.codeSegment[ip][1], self.codeSegment[ip][2]
                dirResult =  self.codeSegment[ip][3]

                memRes = self.getMemType(dirResult)
                valOper1, valOper2 = self.getOperandsValue(operand1, operand2)
                aux = valOper1 / valOper2
                self.tempMemory.assign_value(dirResult, aux)
                ip +=1
            elif codeOp == 8:
                # Get dir addresses
                operand = self.codeSegment[ip][1]
                dirResult =  self.codeSegment[ip][3]
                # Get memory type
                memRes = self.getMemType(dirResult)

                # Get value
                valOper = self.getOperandValue(operand)
                # Assign
                memRes.assign_value(dirResult, valOper)
                ip +=1
            elif codeOp == 9:
                operand = self.codeSegment[ip][3]
                valOper = self.getOperandValue(operand)
                print(valOper)
                ip +=1
            elif codeOp == 10:
                # print(ip)
                # print(self.codeSegment[ip][3])
                ip = self.codeSegment[ip][3] - 1
            elif codeOp == 11:
                conditionDir = self.codeSegment[ip][1]
                false_dest = self.codeSegment[ip][3]

                condition = self.tempMemory.get_value(conditionDir)

                if(condition):
                    ip += 1
                else:
                    ip = false_dest - 1
            elif codeOp == 12:
                conditionDir = self.codeSegment[ip][1]
                true_dest = self.codeSegment[ip][3]
                condition = self.tempMemory.get_value(conditionDir)
                if(condition):
                    ip = true_dest - 1
                else:
                    ip += 1
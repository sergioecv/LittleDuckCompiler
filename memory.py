class Memory:
    def __init__(self):
        self.memory_int = []
        self.memory_float = []
        self.memory_bool = []

        self.dir_range_values = {
            'int':         1000,
            'float':       2000,
            'bool':        3000,
        }

        self.dir_int = 1000;
        self.dir_float = 2000;
        self.dir_bool = 3000;

    def get_dir_range(self):
        dir_range = {
            'int':  self.dir_int,
            'float': self.dir_float,
            'bool': self.dir_float,
        }
        return dir_range
    
    def get_memory_info(self, memory_code):
        for memory_type, base_code in self.dir_range_values.items():
            if memory_code - base_code < 1000:
                memory_index = memory_code - base_code
                return memory_type, memory_index
        return None, None

    def getMemorySize(self):
        intSize = self.dir_int - self.dir_range_values['int']
        floatSize = self.dir_float - self.dir_range_values['float']
        boolSize = self.dir_bool - self.dir_range_values['bool']
        return intSize, floatSize, boolSize
    
    def setMemorySize(self,sizes):
        IntSize , FloatSize, BoolSize = sizes
        self.memory_int = [None] * IntSize
        self.memory_float = [None] * FloatSize
        self.memory_bool = [None] * BoolSize

    def get_value(self, memory_code):
        # Remove memory type code
        memory_code = memory_code % 10000

        memory_type, memory_index = self.get_memory_info(memory_code)
        if memory_type == 'int':
            if(self.memory_int[memory_index] == None):
                raise ReferenceError(f"Variable not initialized")
            else:
                return self.memory_int[memory_index]
        elif memory_type == 'float':
            if(self.memory_float[memory_index] == None):
                raise ReferenceError(f"Variable not initialized")
            else:
                return self.memory_float[memory_index]
        elif memory_type == 'bool':
            if(self.memory_bool[memory_index] == None):
                raise ReferenceError(f"Variable not initialized")
            else:
                return self.memory_bool[memory_index]
    
    def assign_value(self, memory_code, value):
        # Remove memory type code
        memory_code = memory_code % 10000

        memory_type, memory_index = self.get_memory_info(memory_code)
        if memory_type == 'int':
            self.memory_int[memory_index] = value
        elif memory_type == 'float':
            self.memory_float[memory_index] = value
        elif memory_type == 'bool':
            self.memory_bool[memory_index] = value

    def assign_cte(self, cte_type, cte):
        memory_code= None
        if cte_type == 'int':

            if(cte in self.memory_int):
                index_memory = self.memory_int.index(cte)
                memory_code = index_memory + self.dir_range_values['int']
            else:
                memory_code = self.associate_memory_code('int')
                self.memory_int.append(cte)

        elif cte_type == 'float':

            if(cte in self.memory_float):
                index_memory = self.memory_float.index(cte)
                memory_code = index_memory + self.dir_range_values['float']
            else:
                memory_code = self.associate_memory_code('float')
                self.memory_float.append(cte)

        elif cte_type == 'bool':

            if(cte in self.memory_bool):
                index_memory = self.memory_bool.index(cte)
                memory_code = index_memory + self.dir_range_values['bool']
            else:
                memory_code = self.associate_memory_code('bool')
                self.memory_bool.append(cte)
        return memory_code

    def associate_memory_code(self, res_type):
        memory_code = None
        if(res_type == 'int'):
            memory_code = self.dir_int
            self.dir_int += 1
        elif(res_type == 'float'):
            memory_code = self.dir_float
            self.dir_float += 1
        elif(res_type == 'bool'):
            memory_code = self.dir_bool
            self.dir_bool += 1
        return memory_code
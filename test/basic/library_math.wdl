# A library of individual tasks. The plan is to import this file into
#the workflows. This will avoid creating duplicate applets, and reduces compilation
#time.
version 1.0

task Add {
    input {
        Int a
        Int b
    }

    command {
        echo $((${a} + ${b}))
    }
    output {
        Int result = read_int(stdout())
    }
}

task Multiply {
    input {
        Int a
        Int b
    }

    command {
        echo $((a * b))
    }
    output {
        Int result = a * b
    }
}

task Inc {
    input {
        Int i
    }

    command <<<
        python -c "print(${i} + 1)"
    >>>
    output {
        Int result = read_int(stdout())
    }
}

task Sum {
    input {
        Array[Int] ints
    }

    command <<<
        python -c "print(${sep="+" ints})"
    >>>
    output {
        Int result = read_int(stdout())
    }
}

task Twice {
    input {
        Int i
    }

    command <<<
        python -c "print(${i} * 2)"
    >>>
    output {
        Int result = read_int(stdout())
    }
}

task Mod7 {
    input {
        Int i
    }

    command <<<
        python -c "print(${i} % 7)"
    >>>
    output {
        Int result = read_int(stdout())
    }
}

task IntOps {
    input {
        Int a
        Int b
    }

    command {
    }
    output {
        Int mul = a * b
        Int sum = a + b
        Int sub = a - b
        Int div = a / b
        Int ai = a
        Int bi = b
        Int result = a * b + 1
    }
}

# Create a complex number with a WDL object
task ComplexGen {
    input {
        Int a
        Int b
    }

    command <<<
python <<CODE
print('\t'.join(["a", "b"]))
print('\t'.join(["${a}", "${b}"]))
CODE
>>>
    output {
        Object result = read_object(stdout())
    }
}

# Add to complex numbers represented as objects
task ComplexAdd {
    input {
        Object y
        Object z
    }

    command <<<
python <<CODE
a = int(${y.a}) + int(${z.a})
b = int(${y.b}) + int(${z.b})
print('\t'.join(["a","b"]))
print('\t'.join([str(a), str(b)]))
CODE
>>>
    output {
        Object result = read_object(stdout())
    }
}

# Create an array of integers from an integer.
task RangeFromInt {
    input {
        Int len
    }
    command {}
    output {
        Array[Int] result = range(len)
    }
}

# checking behavior with empty arrays
task ArrayLength {
    input {
        Array[Int] ai
    }
    command {}
    output {
        Int result = length(ai)
    }
}

task MaybeInt {
    input {
        Int? a
    }
    command {
    }
    output {
        Int? result = a
    }
}

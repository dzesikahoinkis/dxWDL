# A trivial script, to test the basic sanity
# of a dxWDL release.
version 1.0

import "library_math.wdl" as lib

workflow trivial {
    input {
        Int x = 3
        Int y = 5
    }

    call lib.Add as Add {
        input: a=x, b=y
    }
    output {
        Int sum = Add.result
    }
}

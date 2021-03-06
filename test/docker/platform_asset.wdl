workflow platform_asset {
    call image
    output {
        String result = image.result
    }
}

task image {
    command <<<
        echo "Major Major"
    >>>
    runtime {
        docker: "dx://dxWDL_playground:/ubuntu"
    }
    output {
        String result = read_string(stdout())
    }
}

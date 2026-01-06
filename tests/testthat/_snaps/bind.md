# cbind/rbind reject rank > 2 inputs with clear errors

    Code
      capture_bind_error(r2f(bad_cbind))
    Output
      cbind() only supports rank 0-2 inputs 
    Code
      capture_bind_error(r2f(bad_rbind))
    Output
      rbind() only supports rank 0-2 inputs 


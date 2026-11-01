# explicit choices are required and errors are informative

    Code
      build_network(toy_documents(), toy_citations(), "doc", "ref",
      projection_method = "structured", cooccurrence_method = "cosine")
    Condition
      Error in `build_dynamic_networks()`:
      ! With `projection_method = "structured"`, choose one value for `cooccurrence_method`.
      i Possible values: "coupling_angle", "coupling_strength", and "coupling_similarity".


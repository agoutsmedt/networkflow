# explicit choices are required and errors are informative

    Code
      build_network(toy_documents(), toy_citations(), "doc", "ref", method = "cosine")
    Condition
      Error in `build_network()`:
      ! Choose one value for `method` ("cosine" is not a method).
      i Similarity measures of biblionetwork, which weight the edges: "angle", "strength", "similarity", "jaccard", and "vladutz".
      i Statistical backbones of backbone, which keep the significant edges (with `alpha`): "sdsm", "fdsm", "fixedfill", "fixedrow", and "fixedcol".
      i See the details of `build_dynamic_networks()` for the origin of each method.


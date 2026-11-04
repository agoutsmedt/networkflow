# arguments are checked

    Code
      merge_dynamic_clusters(windows, "cluster", "id", method = "majority",
        threshold_similarity = 0.5001)
    Condition
      Error in `merge_dynamic_clusters()`:
      ! To compare clusters across windows, choose one value for `similarity_type`.
      i Possible values: "complete" and "partial".


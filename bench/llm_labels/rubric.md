# Rubric: quality of the label of a cluster (name_clusters, method = "llm")

Version 1 (2026-11-07). Used word for word by every judge; never edited during a round.

## What you see

For each cluster of a network of academic articles (linked by shared references):
the number of nodes, the titles of its 15 most central articles (with year and journal),
the titles of 4 articles of each other cluster of the same network (to judge
specificity), and a numbered list of candidate labels (`label_id`). You do not know
who or what wrote the labels: judge the words only.

## What to score (each candidate label, independently)

**accuracy** (1-5): does the label describe what most of the 15 titles have in common?
- 5 = names the dominant topic correctly and precisely;
- 4 = correct, slightly vague or slightly incomplete;
- 3 = partly right (too broad, or fits only some of the titles);
- 2 = mostly off (a side topic, or a method/field word that says little);
- 1 = wrong or misleading.

**specificity** (1-5): would a reader of this label alone recognise this cluster among
the other clusters of the network?
- 5 = clearly identifies this cluster, could not be confused with the others;
- 3 = narrows the field down but could fit two clusters;
- 1 = generic (e.g. a whole discipline or "analysis", "models", "studies"), fits any cluster.

A label can be accurate and unspecific ("Monetary policy" in a network about monetary
economics), but a wrong label cannot be specific: if accuracy is 1 or 2, specificity is at most 2.

Do not reward length: a clear two-word label can score 5. Do not score the style
(capitals, number of words); only content.

## Output

Write a CSV file with the header `sample,cluster,label_id,accuracy,specificity,reason`,
one row per (cluster, label_id), `reason` = at most 12 words, no comma (use `;`).
Use only the information shown; do not use outside knowledge of the network.

## Example (hard case)

Titles: "The Phillips curve and the natural rate", "Inflation expectations and Calvo pricing",
"Sticky prices in a DSGE model", ... Others: exchange rates, banking, labour search.
- "Inflation dynamics" -> accuracy 5, specificity 4 (fits this cluster only).
- "Monetary economics" -> accuracy 3, specificity 1.
- "Phillips curve" -> accuracy 4, specificity 5 (narrower than the cluster, yet identifies it).
- "Exchange rate regimes" -> accuracy 1, specificity 1.

# Updates AUG26

- Added positive and negative scoring support
- added bonus problem support for either positive or negative scoring modes
- fixed a minus one counting error in summary statistics of cut data
- updated cut data to show negative points and positive points attributed to the cut

## test data set

- For both problems I took a 13point writ (N2L writ gradescope PH201 MAJ Filip Section8 Spring 26)
- 13 points (UCM problem went to a 'bonus')
- 57 submissions (all but garcia)
- average 12.32/13
- minimum 8
- median 13
- max 15

UCM bonus problem has (part 1):
+2 for correct
+.5 for partial correctness

FBD problem for distribution verification (part 2):
47% correct
9% does not isolate body
9% more than one missing/incorrect force
2% no rotated coordinates
46% no/incorrect critical angles

## Bonus problem doesn't work (part 1)

- Three separate issues it could be I need to test:
  - With normal negative scoring (but with positive values), but achieving numbers out of 0 (bonus), where does the app break?
    - It was a flag on the question generation in the brief from gradescope
  - With positive scoring does it work at all?
  - With positive scoring can I implement bonus problem?

## summing counts is off (part 2)

- using the FBD problem distribution
- Verify the numbers calculated for cut values are correct, find where the issue is.
- Issue was an additional -1 introduced in the code.
-

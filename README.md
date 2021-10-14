README

# Survey123_to_HEPDATA

Process and screen ACR Heron and Egret Monitoring Project data that was
entered in Survey123; end goal is screened data for input into HEPDATA.
Processing the data from Survey123 to HEPDATA is comprised of 6 steps

Each of these steps involves multiple sub-steps, and each major step has
its own code file where the functions and processes for each sub-step
are defined. Users generally will not need to access or open the major
step code files. Rather, all the functions can be loaded with source(),
and the file Survey123_to_HEPDATA_workflow.R should serve as a
vignette with examples and instructions to call the functions for the
entire workflow.

Each step ends with writing files to disk, and subsequent steps read those written files back in. Thus, the workflow can be paused between steps. If you copy these code files locally, it will probably only work if you also create a local RStudio project in a location with the following folder structure:

  - Survey123_to_HEPDATA
      - code
      - data
          - downloaded
          - screened
          - track_changes
          - wrangled
      - season_summary_forms
          - 2020
          - 2021
          - ->more years as needed
          - sheet_creation_logs
      - Survey123_to_HEPDATA.Rproj

Some notes on the logic and motivation for this workflow. The screening
and data management process described here was designed to mimic the
prior screening process as much as possible. The important aspects of
the prior process that we wanted to retain were 1: to have the data
viewable in a Season Summary Sheet-like format; 2: have those data
editable; and 3: have those edits carry forward and be trackable. This
motivation created some constraints, and these constraints in part led
to the somewhat complicated workflow described here.

However, it should be noted that this data structure is consistent
throughout most of the steps in this new process. Thus, if at a later
date it is decided that the manual screening is not needed, then it will
be fairly straightforward to eliminate steps 2, 3, and 4. Those steps
could perhaps be replaced by a single automated screening step. Or, the
output from step 1 can be used as the input for step 6 if for some
reason it is decided that no data screening is necessary.


Log of data processing activities so the code versions used each year can be referenced/reused if needed:
2020 - all processed by 1 Oct 2021

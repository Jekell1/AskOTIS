      * COPYMEMBER: LIBLP/LPMDTEW
      *--------------------------------------------------------
      *            (LPMDTEW)
      *   MATURITY-DATE CALCULATION WORKERS
      *
      *  REV:
      *   032188 JTG ADDED MDTE-FLAG REMOVED MDTE-DATE-WORK
      *   051089 JTG CHANGED WORKERS TO COMP
      *   080990 JTG ADDED MDTE-ORGTERM-FG RE: ADDON INS.
      *   050218 BAH ADDED MDTE-DC-FLAG FOR DUE DATE CHANGES TO NOT AFFECT
      *              MATURITY DATE...LENDMARK PL# 463
      *--------------------------------------------------------
       01  MDTE-DATE       PIC 9(8)  COMP   VALUE 0.
       01  MDTE-FLAG       PIC X            VALUE " ".
       01  MDTE-ORGTERM-FG PIC X            VALUE " ".
       01  MDTE-DC-FLAG    PIC X            VALUE " ".

      * COPYMEMBER: LIBLP/LPXDTE
      **************************************************************
      *         EXPIRATION DATE CALCULATION
      *
      *   NAME: LPXDTE
      *   DESC: COMPUTES THE EXPIRATION DATE OF THE LOAN.
      *   IN  :
      *   OUT : XDTE-DATE
      *   USED: SP-EXREDFAC, SP-EXPD-FRMLA
      *   COPY: LPXDTEW, LPNDTE, LPNDTEW, LPMDTE, LPMDTEW.
      *
      * REV:
      *  JTG 032188 ADDED LOGIC FOR OPTIONAL EXPIRATION
      *             DATES VIA. SP-EXPD-FRMLA
      *  SLC 042888 TOOK OUT OPTION FOR SP-EXPD-FRMLA = "B".
      *             ADDED LOGIC TO INCLUDE EXP. DAYS.
      *  JTG 071896 CHANGED TO WORK WITH NEW DATE ROUTINES
      *  BAH 050218 ADDED SP-EXPD-FRMLA = "B", LENDMARK PL# 463
      *             DONT ALLOW DUE DATE CHANGES TO AFFECT MATURITY
      **************************************************************
       EXPIRATION-DATE-CALCULATION SECTION.
           IF SP-EXPD-FRMLA = "A" OR "B"
              MOVE "Y" TO MDTE-FLAG.
           IF SP-EXPD-FRMLA = "B"
              MOVE "Y" TO MDTE-DC-FLAG.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO NDTE-DATE.
           MOVE SP-EXPD-MONTHS TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.

           IF SP-EXPD-DAYS NOT = 0
              MOVE SP-EXPD-DAYS TO NDTE-HOLD
              PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO XDTE-DATE.

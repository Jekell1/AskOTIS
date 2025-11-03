      ******************************************************
      *    GET ALL INSURANCE PREMIUMS
      ******************************************************
       INDU-GET-ALL-INSPREM SECTION.
           MOVE 0 TO INDU-ALL-INSPREM.
           IF LN-INSOURS(1) NOT = "M"
              ADD LN-INSPREM(1) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(2) NOT = "M"
              ADD LN-INSPREM(2) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(3) NOT = "M"
              ADD LN-INSPREM(3) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(4) NOT = "M"
              ADD LN-INSPREM(4) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(5) NOT = "M"
              ADD LN-INSPREM(5) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(6) NOT = "M"
              ADD LN-INSPREM(6) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(7) NOT = "M"
              ADD LN-INSPREM(7) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(8) NOT = "M"
              ADD LN-INSPREM(8) TO INDU-ALL-INSPREM.
       INDU-GET-ALL-INSPREM-EXIT.
           EXIT.

      *================================================================*
      * END COPYBOOK: LIBLP\LPRATE.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPXDTE.CPY                           *
      *================================================================*
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

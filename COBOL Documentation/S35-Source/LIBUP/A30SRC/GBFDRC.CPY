      * COPYMEMBER: LIBGB/GBFDRC
      *****************************************************************
      *            ( GLOBAL RECAP (DAILY) FILE )   
      *            KEY 1  - BRANCH / DATE / DAY OPEN FOR  
      *
      * NOTE:
      *   RC-TRANS-DATE - IS THE TRANSACTION DATE POSTED FOR
      *                     (YYYMMDD)
      * REV:
      *  BLL 062195 ADDED RC-STATUS OF M1 (CHECKS EXTRACTED)
      *  BLL 090595 ADDED RC-STATUS OF G1 (EOM EARNINGS DONE),
      *             RC-EARNINGS-DATE, EARNINGS-TIME, LAST-ACCRUE
      *****************************************************************
       FD  ORC-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/GB01RC.CPY" REPLACING LEADING "RC" BY "ORC".

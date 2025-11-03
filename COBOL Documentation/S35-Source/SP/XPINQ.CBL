       IDENTIFICATION DIVISION.
       PROGRAM-ID.      XPINQ.
       DATE-WRITTEN.    013184.
      ****************************************************************
      *
      *  DESCRIPTION:  COLLECTION WORK SCREEN
      *
      *   NOTE: BOTH SPINQ.C AND XPINQ.C SHARE THE
      *         CODE FOUND IN LIBSP/SPINQ1(BEG OF PROGRAM THRU
      *                                    WORKING STORAGE)
      *                     & LIBSP/SPINQ2 (DECLARATIVES THRU END)
      *         LINKAGE IS DIFFERENCE BECAUSE WI/BWLNS3 CALLS
      *         XPINQ & PASSES SELECTED LOAN # IN LINKAGE.
      *
       COPY "LIBSP/SPINQ1.CPY".
      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       COPY "LIBGB/GETFMW.CPY".
       01  BWLNS3-LINK-BUF.
           03  BWLNS3-SCANYN      PIC X(1).
           03  BWLNS3-LINK-ACCTNO PIC 9(8).

       SCREEN SECTION.
       COPY "LIBSP/SPINQ_SCN.CPY".
       COPY "LIBWI/NXCNTC_SCN.CPY".
       COPY "LIBWI/NXCNT2_SCN.CPY".
       COPY "LIBWI/INBRT2_SCN.CPY".
       COPY "LIBWI/WRLDBK_SCN.CPY".


       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME BWLNS3-LINK-BUF.
       COPY "LIBSP/SPINQ2.CPY".

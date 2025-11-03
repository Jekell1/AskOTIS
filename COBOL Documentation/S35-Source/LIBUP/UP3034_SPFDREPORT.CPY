      * COPYMEMBER: LIBUP/UP3032_SPFDREPORT.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBUP/UP3032_SPFDREPORT.CPY
      *
      * DESCRIPTION   : 'FD' FOR REPORT-FILE FOR PRINTING REPORT
      *
      *=================================================================
      * REV:
      * KEC 2018-0918 ***NEW*** LOCAL-TO-GLOBAL L2G
      *
      ******************************************************************
      *
      *    R E P O R T - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR PRINTING REPORT
      ******************************************************************
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD.

      *-----------------------------------------------------------------
       01  REPORT-REC.
           03  REPORT-DETAIL-LINE         PIC X(132).
           03  REPORT-LN-FEED             PIC 9(002).


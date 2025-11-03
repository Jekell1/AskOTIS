      * COPYMEMBER: LIBUP/UP3234_SPFSREPORT.CPY
      ******************************************************************
      *
      *  IDENTIFICATION: LIBUP/UP3234_SPFSREPORT.CPY
      *
      *  DESCRIPTION   : 'FS' FOR REPORT-FILE FOR PRINTING REPORT
      *
      ******************************************************************
           SELECT REPORT-FILE ASSIGN TO UP3234-REPORT-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY UP3234-REPORT-KEY
                  FILE STATUS FILE-STAT.

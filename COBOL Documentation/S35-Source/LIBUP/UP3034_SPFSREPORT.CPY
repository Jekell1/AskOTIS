      * COPYMEMBER: LIBUP/UP3034_SPFSREPORT.CPY
      ******************************************************************
      *
      *  IDENTIFICATION: LIBUP/UP3034_SPFSREPORT.CPY
      *
      *  DESCRIPTION   : 'FS' FOR REPORT-FILE FOR PRINTING REPORT
      *
      ******************************************************************
           SELECT REPORT-FILE ASSIGN TO UP3034-REPORT-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY UP3034-REPORT-KEY
                  FILE STATUS FILE-STAT.

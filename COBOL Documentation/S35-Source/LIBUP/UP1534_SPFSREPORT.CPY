      * COPYMEMBER: LIBUP/UP1534_SPFSREPORT.CPY
      ******************************************************************
      *
      *  IDENTIFICATION: LIBUP/UP1534_SPFSREPORT.CPY
      *
      *  DESCRIPTION   : 'FS' FOR REPORT-FILE FOR PRINTING REPORT
      *
      ******************************************************************
           SELECT REPORT-FILE ASSIGN TO UP1534-REPORT-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY UP1534-REPORT-KEY
                  FILE STATUS FILE-STAT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      HELP.
       AUTHOR.                          MJD.
       DATE-WRITTEN.                    2000/07/14.

      ******************************************************************
      *                       HELP WINDOW
      *
      * CALLED FROM LIBGB/SCREEN
      * USES LIBGB/HELPLINK {LINKAGE BUFFER}
      *
      * REV:
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                 COMPUTER-NAME.
       OBJECT-COMPUTER.                 COMPUTER-NAME.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSHE.CPY".
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDHE.CPY".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(35) VALUE
           "@(#)GB/HELP S35 05/18/22-11:12:37 >".
       COPY "LIBGB/GBWSHE.CPY".
       COPY "LIBGB/HELP.CPY".

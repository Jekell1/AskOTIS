       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      HELP2.
       AUTHOR.                          MJD.
       DATE-WRITTEN.                    2000/07/14.

      ******************************************************************
      *                       HELP2 WINDOW
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
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)GB/HELP2 A30 02/28/17-14:56:49 >".
       COPY "LIBGB/GBWSHE.CPY".
       COPY "LIBGB/HELP.CPY".

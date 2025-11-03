      * COPYMEMBER: LIBGB/GB01MS
      ******************************************************************
      *
      *   IDENTIFICATION: LIBGB/GB01MS                          RSZ=0450
      *
      *   DESCRIPTION   : MESSAGE FILE
      *
      *-----------------------------------------------------------------
      *   KEY 1: TYPE, ID, BRANCH, MISC-KEY
      *
      *          TYPE:
      *           DT - DATE RANGE 
      *           MO - MONTHLY: 2 DIGIT MONTH
      *           SQ - SEQUENCE: CONSTANT OR CUSTOM AS SPECIFIED
      *                BY PROGRAM
      *           MY - MONTH/YEAR
      *
      *-----------------------------------------------------------------
      * REV:
      * GLF 020899 ***NEW***                                    RSZ=0450
      *
      ******************************************************************
       78  MS-SIZE                                            VALUE 450.
      *-----------------------------------------------------------------
       01  MS-REC.
           03  MS1-KEY.
               05  MS-TYPE                     PIC X(02).
                   88  MS-TYPE-DATE-RANGE          VALUE "DT".
                   88  MS-TYPE-MONTHLY             VALUE "MO".
                   88  MS-TYPE-SEQUENCE            VALUE "SQ".
                   88  MS-TYPE-MONTH-YEAR          VALUE "MY".
                   88  MS-TYPE-VALID               VALUE "DT" "MO" 
                                                         "SQ" "MY".

               05  MS-ID                       PIC X(10).
               05  MS-BRANCH                   PIC 9(04).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  MS-MISC-KEY                 PIC X(12).

               05  MS-MISC-KEY-DAILY           REDEFINES MS-MISC-KEY.
                   07  MS-MISC-KEY-BEG-DATE    PIC 9(06).
                   07  MS-MISC-KEY-END-DATE    PIC 9(06).

               05  MS-MISC-KEY-MONTHLY-YEARLY  REDEFINES MS-MISC-KEY.
                   07  MS-MISC-KEY-MONTH       PIC 9(02).
                   07  MS-MISC-KEY-YEAR        PIC 9(02).
                   07  FILLER                  PIC X(08).

               05  MS-MISC-KEY-SEQUENCE-RDF    REDEFINES MS-MISC-KEY.
                   07  MS-MISC-KEY-SEQUENCE    PIC 9(04).
                   07  FILLER                  PIC X(08).

      *=================================================================
           03  MS-ROWS                         PIC 9(02).    
               88  MS-ROWS-40-COLUMNS              VALUE 01 THRU 10.
               88  MS-ROWS-60-COLUMNS              VALUE 01 THRU 06.
               88  MS-ROWS-80-COLUMNS              VALUE 01 THRU 05.
               88  MS-ROWS-VALID                   VALUE 01 THRU 10.

           03  MS-COLUMNS                      PIC 9(02).    
               88  MS-COLUMNS-40                   VALUE 40.
               88  MS-COLUMNS-60                   VALUE 60.
               88  MS-COLUMNS-80                   VALUE 80.
               88  MS-COLUMNS-VALID                VALUE 01 THRU 80.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  MS-MESSAGE                      PIC X(400).
           03  MS-MESSAGE-40                   REDEFINES MS-MESSAGE
                                               PIC X(40)    OCCURS 10.
           03  MS-MESSAGE-60-RDF               REDEFINES MS-MESSAGE.
               05  MS-MESSAGE-60               PIC X(60)    OCCURS 6.
               05  FILLER                      PIC X(40).
           03  MS-MESSAGE-80                   REDEFINES MS-MESSAGE
                                               PIC X(80)    OCCURS 5.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  FILLER                          PIC X(18).

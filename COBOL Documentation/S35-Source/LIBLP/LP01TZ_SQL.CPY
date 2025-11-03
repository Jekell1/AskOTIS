      * COPYMEMBER: LIBLP/LP01TZ_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - TIME ZONE FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TZ1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  TZ1-CURSOR-STAT-GOOD              VALUE "00".
           88  TZ1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TZ1-RETURN-CODE                 PIC S9(4)   COMP-5.

       01  QTZ1-WHERE-BEG-KEY.
           03  QTZ1-WBEG-CODE              PIC 9(03).
           03  QTZ1-WBEG-PREFIX            PIC 9(03).
 
       01  QTZ1-WHERE-END-KEY.
           03  QTZ1-WEND-CODE              PIC 9(03).
           03  QTZ1-WEND-PREFIX            PIC 9(03).

       01  QTZ1-WHERE-SCAN-BEG-KEY.
           03  QTZ1-WSBEG-CODE             PIC 9(03).
 
       01  QTZ-REC.
           03  QTZ-CODE                PIC 9(03).
           03  QTZ-PREFIX              PIC 9(03).
           03  QTZ-STATE               PIC X(02).
           03  QTZ-DESC                PIC X(40).
           03  QTZ-ZONE                PIC X(01).
           03  QTZ-FACTOR              PIC S9(02).
           03  QTZ-DLS                 PIC X(01).
           03  QTZ-LTOUCH-DATE         PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.

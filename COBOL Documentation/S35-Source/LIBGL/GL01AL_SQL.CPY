      * COPYMEMBER: LIBGL/GL01AL_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - G/L ALLOCATION FILE 
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  AL1-CURSOR-STAT              PIC X(2)  VALUE SPACES.
           88  AL1-CURSOR-STAT-GOOD               VALUE "00".
           88  AL1-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                        "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  AL1-CURSOR-GR-STAT           PIC X(2)  VALUE SPACES.
           88  AL1-CURSOR-GR-STAT-GOOD            VALUE "00".
           88  AL1-CURSOR-GR-STAT-BAD             VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  AL1-RETURN-CODE           PIC S9(4)   COMP-5.

       01  QAL1-WHERE-BEG-KEY.
           03  QAL1-WBEG-ID           PIC X(12).
 
       01  QAL1-WHERE-END-KEY.
           03  QAL1-WEND-ID           PIC X(12).
 
       01  QAL-REC.
           03  QAL-ID                PIC X(12).
           03  QAL-LTOUCH-DATE       PIC X(10).
           03  QAL-DESC              PIC X(30).
           03  QAL-NO-ACCTS          PIC 99.
           03  QAL-ACCT-1             PIC 9(11).
           03  QAL-PCT-1              PIC S9(3)V99.
           03  QAL-ACCT-2             PIC 9(11).
           03  QAL-PCT-2              PIC S9(3)V99.
           03  QAL-ACCT-3             PIC 9(11).
           03  QAL-PCT-3              PIC S9(3)V99.
           03  QAL-ACCT-4             PIC 9(11).
           03  QAL-PCT-4              PIC S9(3)V99.
           03  QAL-ACCT-5             PIC 9(11).
           03  QAL-PCT-5              PIC S9(3)V99.
           03  QAL-ACCT-6             PIC 9(11).
           03  QAL-PCT-6              PIC S9(3)V99.
           03  QAL-ACCT-7             PIC 9(11).
           03  QAL-PCT-7              PIC S9(3)V99.
           03  QAL-ACCT-8             PIC 9(11).
           03  QAL-PCT-8              PIC S9(3)V99.
           03  QAL-ACCT-9             PIC 9(11).
           03  QAL-PCT-9              PIC S9(3)V99.
           03  QAL-ACCT-10            PIC 9(11).
           03  QAL-PCT-10             PIC S9(3)V99.
           03  QAL-TYPE              PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.

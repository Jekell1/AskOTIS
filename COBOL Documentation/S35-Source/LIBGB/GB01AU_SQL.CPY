      * COPYMEMBER: LIBGB/GB01AU_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - AUDIT CHANGES FILE AUFILE
      *
      *=================================================================
      * REV:
      * BAH 2022-0218 #1542
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  AU1-CURSOR-STAT              PIC X(2)  VALUE SPACES.
           88  AU1-CURSOR-STAT-GOOD               VALUE "00".
           88  AU1-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  AU1-RETURN-CODE              PIC S9(4)   COMP-5.
    
       01  QAU1-WHERE-BEG-KEY.
           03  QAU1-WBEG-FILE-NAME      PIC X(07).
           03  QAU1-WBEG-DATE           PIC X(10).
           03  QAU1-WBEG-SEQ            PIC 9(08).

       01  QAU1-WHERE-END-KEY.
           03  QAU1-WEND-FILE-NAME      PIC X(07).
           03  QAU1-WEND-DATE           PIC X(10).
           03  QAU1-WEND-SEQ            PIC 9(08).

       01  QAU-REC.
           03  QAU-FILE-NAME             PIC X(7).
           03  QAU-DATE                  PIC X(10).
           03  QAU-SEQ                   PIC 9(8).
           03  QAU-ACTION                 PIC X.
           03  QAU-FILE-TYPE              PIC X(2).
           03  QAU-BRANCH                 PIC 9(4).
           03  QAU-FILE-KEY               PIC X(50).
           03  QAU-FIELD-DESC             PIC X(30).
           03  QAU-ORIG-VALUE             PIC X(80).
           03  QAU-NEW-VALUE              PIC X(80).
           03  QAU-TICKET                 PIC X(20).
           03  QAU-PROGRAM                PIC X(8).
           03  QAU-TIME                   PIC 9(6).
           03  QAU-USERID                 PIC X(8).
           03  QAU-LTOUCH-DATE            PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.

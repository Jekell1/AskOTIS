      * COPYMEMBER: LIBGL/GL01GL_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - G/L MASTER FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  GL1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  GL1-CURSOR-STAT-GOOD              VALUE "00".
           88  GL1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  GL1-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  GL1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  GL1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  GL2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  GL2-CURSOR-STAT-GOOD              VALUE "00".
           88  GL2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  GL1-RETURN-CODE           PIC S9(4)   COMP-5.
       01  GL2-RETURN-CODE           PIC S9(4)   COMP-5.

       01  QGL1-WHERE-BEG-KEY.
           03  QGL1-WBEG-BR-ACCOUNT        PIC 9(11).
           03  QGL1-WBEG-BR-ACCOUNTX REDEFINES QGL1-WBEG-BR-ACCOUNT.
               05  QGL1-WBEG-BRANCH        PIC 9(4).
               05  QGL1-WBEG-ACCOUNT       PIC 9(7).
 
       01  QGL1-WHERE-END-KEY.
           03  QGL1-WEND-BR-ACCOUNT        PIC 9(11).
           03  QGL1-WEND-BR-ACCOUNTX REDEFINES QGL1-WEND-BR-ACCOUNT.
               05  QGL1-WEND-BRANCH        PIC 9(4).
               05  QGL1-WEND-ACCOUNT       PIC 9(7).
 
       01  QGL2-WHERE-BEG-KEY.
           03  QGL2-WBEG-DESC              PIC X(36).
 
       01  QGL2-WHERE-END-KEY.
           03  QGL2-WEND-DESC              PIC X(36).

       01  QGL-REC.
           03  QGL-FLAG             PIC X.
           03  QGL-BR-ACCOUNT       PIC 9(11).
           03  QGL-LTOUCH-DATE      PIC X(10).
           03  QGL-DESCRIPTION      PIC X(36).
           03  QGL-CONSOL           PIC 9(4).
           03  QGL-OPENBAL          PIC S9(9)V99.
           03  QGL-CYBAL-1          PIC S9(9)V99.
           03  QGL-CYBAL-2          PIC S9(9)V99.
           03  QGL-CYBAL-3          PIC S9(9)V99.
           03  QGL-CYBAL-4          PIC S9(9)V99.
           03  QGL-CYBAL-5          PIC S9(9)V99.
           03  QGL-CYBAL-6          PIC S9(9)V99.
           03  QGL-CYBAL-7          PIC S9(9)V99.
           03  QGL-CYBAL-8          PIC S9(9)V99.
           03  QGL-CYBAL-9          PIC S9(9)V99.
           03  QGL-CYBAL-10         PIC S9(9)V99.
           03  QGL-CYBAL-11         PIC S9(9)V99.
           03  QGL-CYBAL-12         PIC S9(9)V99.
           03  QGL-LYBAL-1          PIC S9(9)V99.
           03  QGL-LYBAL-2          PIC S9(9)V99.
           03  QGL-LYBAL-3          PIC S9(9)V99.
           03  QGL-LYBAL-4          PIC S9(9)V99.
           03  QGL-LYBAL-5          PIC S9(9)V99.
           03  QGL-LYBAL-6          PIC S9(9)V99.
           03  QGL-LYBAL-7          PIC S9(9)V99.
           03  QGL-LYBAL-8          PIC S9(9)V99.
           03  QGL-LYBAL-9          PIC S9(9)V99.
           03  QGL-LYBAL-10         PIC S9(9)V99.
           03  QGL-LYBAL-11         PIC S9(9)V99.
           03  QGL-LYBAL-12         PIC S9(9)V99.
           03  QGL-CYBUD-1          PIC S9(9).
           03  QGL-CYBUD-2          PIC S9(9).
           03  QGL-CYBUD-3          PIC S9(9).
           03  QGL-CYBUD-4          PIC S9(9).
           03  QGL-CYBUD-5          PIC S9(9).
           03  QGL-CYBUD-6          PIC S9(9).
           03  QGL-CYBUD-7          PIC S9(9).
           03  QGL-CYBUD-8          PIC S9(9).
           03  QGL-CYBUD-9          PIC S9(9).
           03  QGL-CYBUD-10         PIC S9(9).
           03  QGL-CYBUD-11         PIC S9(9).
           03  QGL-CYBUD-12         PIC S9(9).
           03  QGL-LYBUD-1          PIC S9(9).
           03  QGL-LYBUD-2          PIC S9(9).
           03  QGL-LYBUD-3          PIC S9(9).
           03  QGL-LYBUD-4          PIC S9(9).
           03  QGL-LYBUD-5          PIC S9(9).
           03  QGL-LYBUD-6          PIC S9(9).
           03  QGL-LYBUD-7          PIC S9(9).
           03  QGL-LYBUD-8          PIC S9(9).
           03  QGL-LYBUD-9          PIC S9(9).
           03  QGL-LYBUD-10         PIC S9(9).
           03  QGL-LYBUD-11         PIC S9(9).
           03  QGL-LYBUD-12         PIC S9(9).
           03  QGL-NYBUD-1          PIC S9(9).
           03  QGL-NYBUD-2          PIC S9(9).
           03  QGL-NYBUD-3          PIC S9(9).
           03  QGL-NYBUD-4          PIC S9(9).
           03  QGL-NYBUD-5          PIC S9(9).
           03  QGL-NYBUD-6          PIC S9(9).
           03  QGL-NYBUD-7          PIC S9(9).
           03  QGL-NYBUD-8          PIC S9(9).
           03  QGL-NYBUD-9          PIC S9(9).
           03  QGL-NYBUD-10         PIC S9(9).
           03  QGL-NYBUD-11         PIC S9(9).
           03  QGL-NYBUD-12         PIC S9(9).
           03  QGL-TYPE             PIC X.
           03  QGL-CLEAR            PIC X.
           03  QGL-NUMBER1          PIC 9(7).
           03  QGL-NUMBER2          PIC 9(7).
           03  QGL-FS-FLAG          PIC X.
           03  QGL-REPORTS-1        PIC X.
           03  QGL-REPORTS-2        PIC X.
           03  QGL-REPORTS-3        PIC X.
           03  QGL-REPORTS-4        PIC X.
           03  QGL-REPORTS-5        PIC X.
           03  QGL-LYOPENBAL        PIC S9(9)V99.
           03  QGL-DLALLOC-GL       PIC X.
           03  QGL-SPANISH-DESC     PIC X(36).

       EXEC SQL END DECLARE SECTION END-EXEC.

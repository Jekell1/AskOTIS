       IDENTIFICATION DIVISION.
       PROGRAM-ID.    JKC001.
       AUTHOR.        JKC.
       DATE-WRITTEN.  10-11-2022.
      ******************************************************************
      *
      * IDENTIFICATION: UT/JKC001.CBL
      *
      * DESCRIPTION   : TEST FOR SQL TEXT INCREMENT ROUTINE
      *                 WHICH IS IN LIBGB/CONNECT_SQL.CPY
      *
      *=================================================================
      * REV:
      * JKC 2022-1011 ***NEW***
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/JKC001 S35 08/02/23-08:47:44 >".

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       01  WS-A                        PIC S9(10)V99 VALUE ZEROES.
       01  WS-B                        PIC S9(10)V99 VALUE ZEROES.
       01  WS-C                        PIC S9(10)V99 VALUE ZEROES.
       01  MESS                        PIC X(60)     VALUE SPACES.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *=================================================================
       01  SYSTEM-BUF.
           03  SYSTEM-BUF-CMD          PIC X(120)    VALUE SPACES.
           03  SYSTEM-BUF-OUTPUT       PIC X(04)     VALUE " >  ".
           03  SYSTEM-BUF-OUTPUT-FILE  PIC X(35)     VALUE SPACES.

       01  SYSTEM-OUTPUT-FILE.
           03  FILLER                  PIC X(09)     VALUE "/USR/TMP/".
           03  FILLER                  PIC X(06)     VALUE "JKC001".

       01  SYSTEM-ECHO-CMD.
           03  FILLER                  PIC X(05)     VALUE "ECHO ".
           03  FILLER                  PIC X(01)     VALUE X"22".
           03  SYSTEM-ECHO-TEXT        PIC X(80)     VALUE SPACES.
           03  FILLER                  PIC X(01)     VALUE X"22".

      *-----------------------------------------------------------------
       01  SHOW-BEG                       PIC X(35)  VALUE SPACES.
       01  SHOW-BEG-01 REDEFINES SHOW-BEG PIC X(01).
       01  SHOW-BEG-02 REDEFINES SHOW-BEG PIC X(02).
       01  SHOW-BEG-03 REDEFINES SHOW-BEG PIC X(03).
       01  SHOW-BEG-04 REDEFINES SHOW-BEG PIC X(04).
       01  SHOW-BEG-05 REDEFINES SHOW-BEG PIC X(05).

       01  SHOW-END                       PIC X(35)  VALUE SPACES.
       01  SHOW-END-01 REDEFINES SHOW-END PIC X(01).
       01  SHOW-END-02 REDEFINES SHOW-END PIC X(02).
       01  SHOW-END-03 REDEFINES SHOW-END PIC X(03).
       01  SHOW-END-04 REDEFINES SHOW-END PIC X(04).
       01  SHOW-END-05 REDEFINES SHOW-END PIC X(05).

      *=================================================================
       COPY "LIBGB/CONNECTW_SQL.CPY".
           03  ORIG-SQL-TEXT                PIC X(35).

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
           MOVE 123 TO WS-A.
           COMPUTE WS-A = WS-B / WS-C.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    INITIALIZE OUTPUT FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           CALL "C$TOLOWER" USING SYSTEM-OUTPUT-FILE, VALUE 9.
           CALL "C$TOLOWER" USING SYSTEM-ECHO-CMD, VALUE 4.
      D    STOP SYSTEM-OUTPUT-FILE.

           MOVE SPACES                     TO SYSTEM-ECHO-TEXT.
           MOVE SYSTEM-ECHO-CMD            TO SYSTEM-BUF-CMD.
           MOVE " > "                      TO SYSTEM-BUF-OUTPUT.
           MOVE SYSTEM-OUTPUT-FILE         TO SYSTEM-BUF-OUTPUT-FILE.
           CALL 'SYSTEM' USING SYSTEM-BUF.

           MOVE " >> "                     TO SYSTEM-BUF-OUTPUT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE " " TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 01  TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "0" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 01  TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "1" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 01  TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "8" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 01  TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "9" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 01  TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "A" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 01  TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "B" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 01  TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "Y" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 01  TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "Z" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 01  TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO SYSTEM-ECHO-TEXT.
           MOVE SYSTEM-ECHO-CMD  TO SYSTEM-BUF-CMD.
           CALL 'SYSTEM' USING SYSTEM-BUF.

           MOVE "AB0" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "AB9" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "ABA" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "ABZ" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "AZZ" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

      D    STOP SQL-TEXT-ALPHA.
           MOVE "ZYZ" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "ZZZ" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO SYSTEM-ECHO-TEXT.
           MOVE SYSTEM-ECHO-CMD  TO SYSTEM-BUF-CMD.
           CALL 'SYSTEM' USING SYSTEM-BUF.

           MOVE "AB0" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 05    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "AB9" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 05    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "ABA" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 05    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "ABZ" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 05    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "AZZ" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 05    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "ZYZ" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 05    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "ZZZ" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 05    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO SYSTEM-ECHO-TEXT.
           MOVE SYSTEM-ECHO-CMD  TO SYSTEM-BUF-CMD.
           CALL 'SYSTEM' USING SYSTEM-BUF.

           MOVE "A 0" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "A 9" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "A A" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "A Z" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "Z Z" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO SYSTEM-ECHO-TEXT.
           MOVE SYSTEM-ECHO-CMD  TO SYSTEM-BUF-CMD.
           CALL 'SYSTEM' USING SYSTEM-BUF.

           MOVE "A 0" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 05    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "A 9" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 05    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "A A" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 05    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "A Z" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 05    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "Z Z" TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 05    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO SYSTEM-ECHO-TEXT.
           MOVE SYSTEM-ECHO-CMD  TO SYSTEM-BUF-CMD.
           CALL 'SYSTEM' USING SYSTEM-BUF.

           MOVE "A9 " TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "AB " TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "AZ " TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "A  " TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

           MOVE "Z  " TO SQL-TEXT ORIG-SQL-TEXT.
           MOVE 03    TO SQL-TEXT-SIZE.
           PERFORM SQL-TEXT-INCREMENT.
           PERFORM LOAD-RESULTS.

      *=================================================================
           DISPLAY " " AT LINE 3.
           DISPLAY "OUTPUT FILE: " SYSTEM-OUTPUT-FILE.
           DISPLAY "PROGRAM COMPLETE. PRESS ENTER TO EXIT." AT LINE 5.
           ACCEPT OMITTED.

           STOP RUN.     *> EXIT PROGRAM

      ******************************************************************
      *
      *    L O A D - R E S U L T S
      *
      ******************************************************************
       LOAD-RESULTS.

           MOVE SPACES        TO SYSTEM-ECHO-TEXT.
           MOVE ORIG-SQL-TEXT TO SHOW-BEG.
           MOVE SQL-TEXT      TO SHOW-END.

           IF ( SQL-TEXT-SIZE = 01 )
              STRING "SIZE = ", SQL-TEXT-SIZE, 
                     "| '", SHOW-BEG-01, "' BECOMES '", SHOW-END-01, "'"
                     DELIMITED BY SIZE INTO SYSTEM-ECHO-TEXT
           ELSE
           IF ( SQL-TEXT-SIZE = 02 )
              STRING "SIZE = ", SQL-TEXT-SIZE, 
                     "| '", SHOW-BEG-02, "' BECOMES '", SHOW-END-02, "'"
                     DELIMITED BY SIZE INTO SYSTEM-ECHO-TEXT
           ELSE
           IF ( SQL-TEXT-SIZE = 03 )
              STRING "SIZE = ", SQL-TEXT-SIZE, 
                     "| '", SHOW-BEG-03, "' BECOMES '", SHOW-END-03, "'"
                     DELIMITED BY SIZE INTO SYSTEM-ECHO-TEXT
           ELSE
           IF ( SQL-TEXT-SIZE = 04 )
              STRING "SIZE = ", SQL-TEXT-SIZE, 
                     "| '", SHOW-BEG-04, "' BECOMES '", SHOW-END-04, "'"
                     DELIMITED BY SIZE INTO SYSTEM-ECHO-TEXT
           ELSE
           IF ( SQL-TEXT-SIZE = 05 )
              STRING "SIZE = ", SQL-TEXT-SIZE, 
                     "| '", SHOW-BEG-05, "' BECOMES '", SHOW-END-05, "'"
                     DELIMITED BY SIZE INTO SYSTEM-ECHO-TEXT
           ELSE
              MOVE "*** ERROR ***" TO SYSTEM-ECHO-TEXT.

           MOVE SYSTEM-ECHO-CMD  TO SYSTEM-BUF-CMD.
           CALL 'SYSTEM' USING SYSTEM-BUF.

      ******************************************************************
      *
      *    S Q L - T E X T - I N C R E M E N T
      *
      *=================================================================
      * IN  : SQL-TEXT
      *       SQL-TEXT-SIZE [SIZE OF THE COBOL FIELD SIZE = PIC X(##)]
      * OUT : SQL-TEXT
      * DESC: USED TO INCREMENT TEXT STRING.  NEED THE SIZE OF THE COBOL
      *       FIELD TO KNOW WHICH CHARACTER NEEDS TO BE INCREMENTED TO.
      *
      *       IF THE LAST CHARACTER (SQL-TEXT-SIZE) IS NOT SPACE, THEN
      *       INCREMENT THAT CHARACTER:
      *
      *         " "     BECOMES "0"
      *         "0"     BECOMES "1"
      *         "9"     BECOMES "A"
      *         "A"     BECOMES "B"
      *         "Z"     BECOMES "Z"   (UPPER Z TO LOWER Z)
      *
      *         "AB0"   BECOMES "AB1"
      *         "AB9"   BECOMES "ABA"
      *         "ABA"   BECOMES "ABB"
      *         "ABZ"   BECOMES "AC " 
      *         "AZZ"   BECOMES "B  " 
      *         "ZYZ"   BECOMES "ZZ " 
      *         "ZZZ"   BECOMES "ZZZ" (LAST CHAR IS LOWER Z)
      *
      *         "AB0  " BECOMES "AB00 " (SPACE TO ZERO)
      *         "AB9  " BECOMES "AB90 " (SPACE TO ZERO)
      *         "ABA  " BECOMES "ABA0 " (SPACE TO ZERO)
      *         "ABZ  " BECOMES "ABZ0 " (SPACE TO ZERO)
      *         "AZZ  " BECOMES "AZZ0 " (SPACE TO ZERO)
      *         "ZYZ  " BECOMES "ZYZ0 " (SPACE TO ZERO)
      *         "ZZZ  " BECOMES "ZZZ0 " (SPACE TO ZERO)
      *
      *         "A 0"   BECOMES "A 1"
      *         "A 9"   BECOMES "A A"
      *         "A A"   BECOMES "A B"
      *         "A Z"   BECOMES "A0 " 
      *         "Z Z"   BECOMES "Z0 " 
      *
      *         "A 0  " BECOMES "A 00 "
      *         "A 9  " BECOMES "A 90 "
      *         "A A  " BECOMES "A A0 "
      *         "A Z  " BECOMES "A Z0 " 
      *         "Z Z  " BECOMES "Z Z0 " 
      *
      *         "A9 "   BECOMES "A90" (SPACE TO ZERO)
      *         "AB "   BECOMES "AB0" (SPACE TO ZERO)
      *         "AZ "   BECOMES "AZ0" (SPACE TO ZERO)
      *
      *         "A  "   BECOMES "A0 " (SPACE TO ZERO)
      *         "Z  "   BECOMES "Z0 " (SPACE TO ZERO)
      *
      ******************************************************************
       SQL-TEXT-INCREMENT.

           IF ( SQL-TEXT-SIZE NOT NUMERIC ) OR
              ( SQL-TEXT-SIZE = ZEROES    )
              MOVE 35 TO SQL-TEXT-SIZE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF TEXT FIELD IS SPACES, THEN MAKE IT A LEADING 0 ("0     ")
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT = SPACES )
              MOVE "0" TO SQL-TEXT
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF TEXT FIELD SIZE (SQL-TEXT-SIZE) IS EQUAL TO 1, THEN:
      *      1. IF NUMERIC (9), MAKE IT 'A'
      *      2. IF NUMERIC (0-8), INCREMENT BY 1 (1-9)
      *      3. IF ALPHA (Z), MAKE IT A LOWER CASE 'Z'
      *      4. IF ALPHA (A-Y), INCREMENT BY 1 (B-Z)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO SQL-TEXT-S1.

           IF ( SQL-TEXT-SIZE = SQL-TEXT-S1 )                *> NUMERIC
              IF ( SQL-TEXT(SQL-TEXT-S1:1) NUMERIC )
                 IF ( SQL-TEXT9(SQL-TEXT-S1) = 9 )
                    MOVE "A" TO SQL-TEXTX(SQL-TEXT-S1)
                 ELSE
                    ADD 01 TO SQL-TEXT9(SQL-TEXT-S1)
                 END-IF
                 EXIT PARAGRAPH.

           IF ( SQL-TEXT-SIZE = SQL-TEXT-S1 )                *> Z
              IF ( SQL-TEXTX(SQL-TEXT-S1) = "Z" )
                 CALL "C$TOLOWER" USING SQL-TEXTX(SQL-TEXT-S1),VALUE 1
                 EXIT PARAGRAPH.

           IF ( SQL-TEXT-SIZE = SQL-TEXT-S1 )                *> A-Y
              MOVE SQL-TEXT-S1     TO SQL-TEXT-S3
              PERFORM SQL-TEXT-INCREMENT-ALPHA
              MOVE SQL-TEXT-CHANGE TO SQL-TEXTX(SQL-TEXT-S1)
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF CHAR FOUND (NON-SPACE/NUMERIC) IS IN THE LAST CHAR OF THE
      *    TEXT FIELD BASED ON THE SIZE (SQL-TEXT-S1 = SQL-TEXT-SIZE)
      *    THEN:
      *      1. IF NUMERIC (9), MAKE IT 'A'
      *      2. IF NUMERIC (0-8), INCREMENT BY 1 (1-9)
      *      3. IF ALPHA (Z):
      *         A. IF PREVIOUS CHARS (SQL-TEXT-S1 MINUS #) ARE ALL 'Z',
      *            MAKE IT A LOWER CASE 'Z' IN LAST BYTE
      *         B. FIND THE PREVIOUS CHAR (SQL-TEXT-S1 MINUS #) THAT
      *            IS NOT 'Z', THEN INCREMENT THAT CHAR BY 1 FOLLOWED
      *            BY ALL SPACES
      *      4. IF ALPHA (A-Y), INCREMENT BY 1 (B-Z)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXTX(SQL-TEXT-SIZE) NOT = SPACES )
              MOVE SQL-TEXT-SIZE TO SQL-TEXT-S1        *> USE NEXT IFS
           ELSE
              MOVE ZEROES        TO SQL-TEXT-S1.       *> SKIP NEXT IFS

           IF ( SQL-TEXT-SIZE = SQL-TEXT-S1 )                *> NUMERIC
              IF ( SQL-TEXT(SQL-TEXT-S1:1) NUMERIC )
                 IF ( SQL-TEXT9(SQL-TEXT-S1) = 9 )
                    MOVE "A" TO SQL-TEXTX(SQL-TEXT-S1)
                 ELSE
                    ADD 01 TO SQL-TEXT9(SQL-TEXT-S1)
                 END-IF
                 EXIT PARAGRAPH.

           IF ( SQL-TEXT-SIZE = SQL-TEXT-S1 )                *> Z
              IF ( SQL-TEXTX(SQL-TEXT-S1) = "Z" )
                 MOVE ZEROES TO SQL-TEXT-S3
                 COMPUTE SQL-TEXT-S2 = SQL-TEXT-SIZE - 1
                 PERFORM VARYING SQL-TEXT-S2 FROM SQL-TEXT-S2 BY -1
                           UNTIL SQL-TEXT-S2 <= 0
                     IF ( SQL-TEXT(SQL-TEXT-S2:1) NOT = "Z" )
                        MOVE SQL-TEXT-S2 TO SQL-TEXT-S3
                        MOVE ZEROES      TO SQL-TEXT-S2
                     END-IF
                 END-PERFORM
                 IF ( SQL-TEXT-S3 <= 0 )
                    MOVE SQL-TEXT-SIZE TO SQL-TEXT-S2
                    MOVE "Z" TO SQL-TEXTX(SQL-TEXT-S2)
                    CALL "C$TOLOWER" USING SQL-TEXTX(SQL-TEXT-S2),
                         VALUE 1
                    COMPUTE SQL-TEXT-S2 = SQL-TEXT-S2 + 1
                 ELSE
                    IF ( SQL-TEXTX(SQL-TEXT-S3) = SPACES )
                       MOVE "0" TO SQL-TEXTX(SQL-TEXT-S3)
                       COMPUTE SQL-TEXT-S2 = SQL-TEXT-S3 + 1
                    ELSE
                       MOVE SQL-TEXT-S3 TO SQL-TEXT-S2
                       PERFORM SQL-TEXT-INCREMENT-ALPHA
                       MOVE SQL-TEXT-CHANGE TO SQL-TEXTX(SQL-TEXT-S2)
                       COMPUTE SQL-TEXT-S2 = SQL-TEXT-S2 + 1
                    END-IF
                 END-IF
                 PERFORM VARYING SQL-TEXT-S2 FROM SQL-TEXT-S2 BY 1
                           UNTIL SQL-TEXT-S2 > 35
                    MOVE SPACES TO SQL-TEXTX(SQL-TEXT-S2)
                 END-PERFORM
                 EXIT PARAGRAPH.

           IF ( SQL-TEXT-SIZE = SQL-TEXT-S1 )                *> A-Y
              MOVE SQL-TEXT-S1 TO SQL-TEXT-S3
              PERFORM SQL-TEXT-INCREMENT-ALPHA
              MOVE SQL-TEXT-CHANGE TO SQL-TEXTX(SQL-TEXT-S1)
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FIND THE BYTE FOR THE LAST NON-SPACE CHAR IN SQL-TEXT
      *    THEN LOAD SQL-TEXT-S1 WITH THAT BYTE + 1
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES TO SQL-TEXT-S1.

           PERFORM VARYING SQL-TEXT-S2 FROM 35 BY -1
                     UNTIL SQL-TEXT-S2 <= 0

                   IF ( SQL-TEXT(SQL-TEXT-S2:1) NOT = SPACES )
                      MOVE SQL-TEXT-S2 TO SQL-TEXT-S1
                      MOVE ZEROES      TO SQL-TEXT-S2
                   END-IF

           END-PERFORM.

           IF ( SQL-TEXT-S1 = ZEROES )   *> NOT FOUND; ALL SPACES
              MOVE "0" TO SQL-TEXTX(1)   *> FORCE 0 TO 1ST CHARACTER
              EXIT PARAGRAPH.

           ADD 01 TO SQL-TEXT-S1.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FOR THE BYTE AFTER THE LAST NON-SPACE CHAR MAKE IT A ZERO"
      *         "123  " BECOMES "1230 " (SPACE TO ZERO)
      *         "AZZ  " BECOMES "AZZ0 " (SPACE TO ZERO)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "0" TO SQL-TEXTX(SQL-TEXT-S1).
           EXIT PARAGRAPH.

      ******************************************************************
      *
      *    S Q L - T E X T - I N C R E M E N T - A L P H A
      *
      *=================================================================
      * IN  : SQL-TEXT
      *       SQL-TEXT-S3     [THE BYTE TO BE INCREMENTED]
      * OUT : SQL-TEXT-CHANGE [THE CHAR TO CHANGE TO]
      * DESC: SEARCH THE ALPHA TABLE TO FIND THE MATCHING CHAR (BYTE)
      *       THEN INCREMENT THAT TO THE NEXT CHAR (BYTE)
      ******************************************************************
       SQL-TEXT-INCREMENT-ALPHA.

           MOVE SPACES TO SQL-TEXT-CHANGE.

           PERFORM VARYING SQL-TEXT-S4 FROM 1 BY 1
                     UNTIL SQL-TEXT-S4 > 25                      *> A-Y

             IF ( SQL-TEXT-ALPHAX(SQL-TEXT-S4) = SQL-TEXTX(SQL-TEXT-S3))
                MOVE SQL-TEXT-ALPHAX(SQL-TEXT-S4 + 1)
                  TO SQL-TEXT-CHANGE
                MOVE 26 TO SQL-TEXT-S4
             END-IF

           END-PERFORM.


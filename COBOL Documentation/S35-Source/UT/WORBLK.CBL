       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WORBLK.
      *****************************************************
      *  DIR : GB
      *  DESC: WINOTIS BLOCK DATA RECEIVE
      *  REV :
      *    JHP 06272018 CREATED.
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/WORBLK S35 03/31/23-09:24:39 >".
       01 RCV-ERR-FLAG                  PIC 9.
       01 CMP-BLOCK-ID                  PIC 9(3) COMP-3.
       01 COMMAND-DATA-LEN              PIC 9(4).
       01 COMMAND-DATA.
           03 COMMAND-DATA-VALUE        PIC X(1400).
           03 COMMAND-DATA-ARRAY  REDEFINES
               COMMAND-DATA-VALUE 
               OCCURS 1400 TIMES        PIC X.

       01 MSG-BUFFER-IDX                PIC 9(5).

       01 PARSE-BUFFER-IDX              PIC 9(4).
       01 PARSE-BUFFER-LEN              PIC 9(4).
       01 PARSE-BUFFER.
           03 PARSE-BLOCK-ID            PIC X(3).
           03 PARSE-BLOCK-LAST          PIC X.
           03 PARSE-BUFFER-VALUE        PIC X(1396).
           03 PARSE-BUFFER-ARRAY REDEFINES
               PARSE-BUFFER-VALUE
               OCCURS 1396 TIMES        PIC X.
       01 ON-BLOCK                      PIC 9(3) COMP-3. 
       01 FIRST-THRU                    PIC 9.
       01 RCV-COMMAND-ID                PIC 9(4).
	   

        
       LINKAGE SECTION.
       01 COMMAND-ID                           PIC 9(4).
       01 MSG-BUFFER-LEN                       PIC 9(4).
       01 MSG-BUFFER.           
           03 MSG-BUFFER-VALUE                 PIC X(16384).
           03 MSG-BUFFER-ARRAY REDEFINES MSG-BUFFER-VALUE
               OCCURS 16384 TIMES              PIC X.        
       01 ERR-FLAG                             PIC 9.

       PROCEDURE DIVISION USING
            COMMAND-ID
            MSG-BUFFER-LEN
            MSG-BUFFER
            ERR-FLAG.

       MAIN-MODULE.
           MOVE 1 TO ERR-FLAG.
           MOVE 1 TO ON-BLOCK.
           MOVE 1 TO FIRST-THRU.
           MOVE ZEROS TO COMMAND-ID.           
           MOVE ZEROS TO MSG-BUFFER-LEN.
           MOVE SPACES TO MSG-BUFFER-VALUE.
           MOVE ZEROS TO MSG-BUFFER-IDX.

       RECEIVE-LOOP.
           MOVE ZEROS TO RCV-COMMAND-ID.
           MOVE ZEROS TO COMMAND-DATA-LEN.
           MOVE SPACES TO COMMAND-DATA-VALUE.
           MOVE ZERO TO RCV-ERR-FLAG.
           CALL "WORCMD" USING
               RCV-COMMAND-ID
               COMMAND-DATA-LEN
               COMMAND-DATA
               RCV-ERR-FLAG.
           CANCEL "WORCMD".
      D    STOP "MSG".
           IF RCV-ERR-FLAG <> 0
               GO TO MAIN-MODULE-EXIT.
           IF COMMAND-DATA-LEN < 4
               MOVE 0 TO MSG-BUFFER-LEN
               MOVE SPACES TO MSG-BUFFER
               MOVE RCV-COMMAND-ID TO COMMAND-ID
               MOVE 0 TO ERR-FLAG
               GO TO MAIN-MODULE-EXIT.
      *     IF FIRST-THRU <> 0 
      *         MOVE 0 TO FIRST-THRU
           MOVE RCV-COMMAND-ID TO COMMAND-ID.
           MOVE COMMAND-DATA-LEN TO MSG-BUFFER-LEN.
           MOVE COMMAND-DATA TO MSG-BUFFER.
           MOVE 0 TO ERR-FLAG.
      *      IF COMMAND-ID <> RCV-COMMAND-ID
      *          GO TO MAIN-MODULE-EXIT.                 
      *      MOVE COMMAND-DATA TO PARSE-BUFFER.
      *      MOVE COMMAND-DATA-LEN TO PARSE-BUFFER-LEN.
      *      SUBTRACT 4 FROM PARSE-BUFFER-LEN
      *          ON SIZE ERROR GO TO MAIN-MODULE-EXIT.
      *      MOVE 0 TO PARSE-BUFFER-IDX.
      *      MOVE PARSE-BLOCK-ID TO CMP-BLOCK-ID.
      *      IF ON-BLOCK <> CMP-BLOCK-ID
      *          GO TO MAIN-MODULE-EXIT.
      *      IF PARSE-BLOCK-LAST <> 'Y' AND
      *          PARSE-BUFFER-LEN < 1396
      *          GO TO MAIN-MODULE-EXIT.               

      *  COPY-DATA-LOOP.
      *      IF PARSE-BUFFER-IDX >= 1396
      *          GO TO COPY-DATA-LOOP-END.
      *      IF PARSE-BUFFER-IDX > PARSE-BUFFER-LEN
      *          GO TO COPY-DATA-LOOP-END.
      *      IF MSG-BUFFER-IDX >= 16384
      *          GO TO COPY-DATA-LOOP-END.
      *      ADD 1 TO PARSE-BUFFER-IDX.
      *      ADD 1 TO MSG-BUFFER-IDX.
      *      MOVE PARSE-BUFFER-ARRAY(PARSE-BUFFER-IDX) TO 
      *          MSG-BUFFER-ARRAY(MSG-BUFFER-IDX).
      *      ADD 1 TO MSG-BUFFER-LEN. 
      *      GO TO COPY-DATA-LOOP. 

      *  COPY-DATA-LOOP-END.
      *      IF PARSE-BLOCK-LAST = 'Y'
      *          MOVE 0 TO ERR-FLAG
      *          GO TO MAIN-MODULE-EXIT.
      *      GO TO RECEIVE-LOOP. 

       MAIN-MODULE-EXIT.
            EXIT PROGRAM.

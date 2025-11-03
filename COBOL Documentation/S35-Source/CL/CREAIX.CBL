       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CREAIX.
       DATE-WRITTEN.    072591.
      ******************************************************************
      *
      * DIRECTORY  : CL
      *
      * DESCRIPTION: CALLED PROGRAM TO CREATE / MODIFY / DELETE
      *                IXFILE RECORDS
      *
      *   BR-COLL-ID-SCAN = "Y" - CREATE IXFILE WITH ALL CHARACTERS
      *                           OF VIN
      *                   = "S" - CREATE IXFILE WITH JUST LAST 6 
      *                           CHARACTERS OF VIN
      * OUTPUT:
      *     IF AN ERROR OCCURS THE CREAIX-ERRCD IS SET NON SPACES.
      *
      * COPY: LIBCL/CREAIXW
      *
      *=================================================================
      * REV:
      * BLL 092195 CHANGE TO READ IX BEFORE REWRITE
      * BAH 071598 FIXED REWRITE ON IX1-FILE, MOVED READ ABOVE IT AND
      *            FILLED IN IX FIELDS AGAIN.
      *  MG 990927 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BAH 2000.0725 DONT GO TO IO-ERROR ON A BAD READ, JUST FIX IT
      *               DEPENDING ON THE ACTION NECESSARY
      * KEC 2017.0221 ADDED IX-BRNO TO IX-REC.
      *               ADDED CREAIX-BRNO TO CREAIX-AREA.
      *               ADDED CREAIX-SIZE FOR USE IN LINKAGE.
      ******************************************************************
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
           "@(#)CL/CREAIX S35 04/29/25-15:09:10 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01IX.CPY".
       COPY "LIBLP/LP01IX_SQL.CPY".
       COPY "LIBLP/LPWSIX.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       01  ERRCD               PIC X.

       01  COLL-ID             VALUE " ".
           03  COLL-ID-ELE     OCCURS 30 TIMES
                               PIC X.
       01  COLL-ID-WK          VALUE " ".
           03  COLL-ID-WK-ELE  OCCURS 30 TIMES
                               PIC X.
       01  VIN-SUB             PIC 9        VALUE 0.
       01  SP-COUNT            PIC 99       VALUE 0.
       01  ID-SUB              PIC 99.
       01  ID-WK-SUB           PIC 99.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/CREAIXW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       01  CREAIX-LINK       PIC X(CREAIX-SIZE).
       PROCEDURE DIVISION
            USING FORM-PATHNAME CREAIX-LINK EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE CREAIX-LINK TO CREAIX-AREA.

           MOVE SPACES TO ERRCD.
           IF NOT (CREAIX-ACTION = "W" OR "R" OR "D")
              MOVE "E" TO ERRCD
              GO TO END-ROUTINE.

      * NEEDED FOR BULK TRANSFERS
           IF CREAIX-IX-PATH NOT = SPACES
              MOVE "Y" TO IX-PATH-OVERRIDE
              MOVE CREAIX-IX-PATH TO IX-PATH.
      * NEEDED FOR BULK TRANSFERS ^

           IF CREAIX-CODE = "C"
              PERFORM OPEN-IX1-FILE
              PERFORM C-TEST-FOR-PURCHASE
              IF ERRCD = " "
                 PERFORM UPDATE-VIN
                 GO TO END-ROUTINE
              ELSE
                 MOVE " " TO ERRCD
                 GO TO END-ROUTINE.

           MOVE "E" TO ERRCD.
           GO TO END-ROUTINE.

      ************************************************
       UPDATE-VIN SECTION.
      ************************************************

      * THE LT COLLATERAL RECORD HAS 3 AUTOS NOW.
      * CREATE AN IX FOR ANY VIN'S THAT EXIST
           IF CREAIX-LTC-VIN(1) NOT = SPACES
              MOVE 1 TO VIN-SUB
              PERFORM UPDATE-IX-FILE.
           IF CREAIX-LTC-VIN(2) NOT = SPACES
              MOVE 2 TO VIN-SUB
              PERFORM UPDATE-IX-FILE.
           IF CREAIX-LTC-VIN(3) NOT = SPACES
              MOVE 3 TO VIN-SUB
              PERFORM UPDATE-IX-FILE.
           PERFORM CLOSE-IX1-FILE.

      ************************************************
       UPDATE-IX-FILE SECTION.
      ************************************************

           PERFORM SETUP-IX-RECORD.

           IF CREAIX-ACTION = "W"
              PERFORM WRITE-IX1-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 GO TO UPDATE-IX-FILE-EXIT.

           PERFORM READ-IX1-FILE.
           IF IO-BAD
              IF CREAIX-ACTION = "R"
                 PERFORM SETUP-IX-RECORD
                 PERFORM WRITE-IX1-FILE
              ELSE
                 IF CREAIX-ACTION = "D"
                    GO TO UPDATE-IX-FILE-EXIT
                 ELSE
                    GO TO IO-ERROR.

           PERFORM SETUP-IX-RECORD.

           IF CREAIX-ACTION = "R"
              PERFORM REWRITE-IX1-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 GO TO UPDATE-IX-FILE-EXIT.

           IF CREAIX-ACTION = "D"
              PERFORM DELETE-IX1-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 GO TO UPDATE-IX-FILE-EXIT.

       UPDATE-IX-FILE-EXIT.
           EXIT.

      ******************************************
       C-TEST-FOR-PURCHASE SECTION.
      ******************************************
           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-GOOD
              IF BR-COLL-ID-SCAN = "Y" OR "S"
                 GO TO C-TEST-FOR-PURCHASE-EXIT.
           MOVE "E" TO ERRCD.

       C-TEST-FOR-PURCHASE-EXIT.
           EXIT.

      ******************************************
       SET-COLL-ID SECTION.
      ******************************************
           IF BR-COLL-ID-SCAN = "Y" OR CREAIX-LTC-VIN(VIN-SUB) = " "
              MOVE CREAIX-LTC-VIN(VIN-SUB) TO COLL-ID
              GO TO SET-COLL-ID-EXIT.

           MOVE CREAIX-LTC-VIN(VIN-SUB) TO COLL-ID-WK.
           PERFORM SET-COLL-ID-COUNT-SP.
           MOVE 1 TO ID-SUB.
           MOVE " " TO COLL-ID.

       SET-COLL-ID-CONT.
           IF ID-SUB < 7
              MOVE COLL-ID-WK-ELE(ID-WK-SUB) TO COLL-ID-ELE(ID-SUB)
              ADD 1 TO ID-SUB ID-WK-SUB
              GO TO SET-COLL-ID-CONT.

           GO TO SET-COLL-ID-EXIT.

       SET-COLL-ID-EXIT.
           EXIT.

      ************************************************
       SET-COLL-ID-COUNT-SP SECTION.
      ************************************************
           MOVE 30 TO SP-COUNT.
       SET-COLL-ID-COUNT-CONT.
           IF SP-COUNT > 6
              IF COLL-ID-WK-ELE(SP-COUNT) = " "
                 SUBTRACT 1 FROM SP-COUNT
                 GO TO SET-COLL-ID-COUNT-CONT.
           SUBTRACT 5 FROM SP-COUNT GIVING ID-WK-SUB.


       COPY "LIBGB/DATER.CPY".

      ******************************************
       MISC SECTION.
      ******************************************
       SETUP-IX-RECORD.
      * "C"
           MOVE CREAIX-BRNO TO IX-BRNO.
           MOVE CREAIX-CODE TO IX-CODE.
           MOVE CREAIX-LTC-ACCTNO TO IX-ACCTNO.
           MOVE VIN-SUB TO IX-SEQNO.

           PERFORM SET-COLL-ID.

           MOVE COLL-ID TO IX-VIN.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-IX1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPIXGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPIX1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           MOVE ERRCD TO CREAIX-ERRCD.
           MOVE CREAIX-AREA TO CREAIX-LINK.
           EXIT PROGRAM.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FINDLN.
       DATE-WRITTEN.    102990.
      ************************************************************
      *                 (CL)
      *  DESCRIPTION:   FIND ALL LOANS WITH PASSED SSNO
      *
      *                 CALLED FROM XONPC0
      *
      * REV:
      *  040496 BAH LOANS MUST BE PAIDOUT WITH THE SAME TRANSDATE
      *             OR SKIP THEM !!
      *  042396 JTG CHANGED TO TEST ON SSNO FIRST
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  081497 BAH ADDED FIND-GIFTNET TO ONLY FIND LOANS THAT ARE OF
      *             THE SAME STATUS, GIFTNET OR NOT GIFTNET, PR # 76
      *  032598 JTG STARRED OUT FOR WORLD TEST,
      *             LOANS MUST BE PAIDOUT WITH THE SAME TRANSDATE
      *             OR SKIP THEM !!
      *  990928  MG FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 20181203 GLOBAL LNFILE
      *  BAH 20220127 COMMENTED OUT GP-I-AM
      *  BAH 20220830 HARD CODED START
      *  BAH 20230707 COMMENT OUT 3RD AND 4TH BORROWER TESTS
      ************************************************************
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
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)CL/FINDLN A30 06/05/13-16:20:00 09/18/14-13:46:56 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  ERRCD           PIC X         VALUE SPACES.

       01  SUB             PIC 9(3) COMP VALUE 0.
       01  LN-COUNT        PIC 9(3) COMP VALUE 0.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/FINDLNW.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME FINDLN-WORKERS EXIT-PATHNAME.

      /***********************************************************
      *    C O N T R O L   M O D U L E
      ************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE 0 TO FOUND-NUMBER(1) FOUND-NUMBER(2) FOUND-NUMBER(3)
                     FOUND-NUMBER(4) FOUND-NUMBER(5).

      D    STOP MESS.
      * TEST 1ST MAKER:
           MOVE 1 TO SUB LN-COUNT.
           PERFORM TEST-SSNO.

      * TEST 2ND MAKER:
           MOVE 2 TO SUB.
           PERFORM TEST-SSNO.

      * TEST 3RD MAKER:
      *    MOVE 3 TO SUB.
      *    PERFORM TEST-SSNO.

      * TEST 4TH MAKER:
      *    MOVE 4 TO SUB.
      *    PERFORM TEST-SSNO.

       UPDATE-EXIT.
           GO TO END-ROUTINE.

      ***************************************************
      *    SEARCH FOR SSNO IN LOANS
      ***************************************************
       TEST-SSNO SECTION.
           PERFORM OPEN-LN4-FILE.
           IF LN-COUNT NOT < 5
              GO TO TEST-SSNO-EXIT.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE FIND-SSNO     TO LN-SSNO(SUB).

           PERFORM START-LN-FILE.
       SSNO-READ-NEXT.
           PERFORM READ-LN-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO TEST-SSNO-EXIT.

           IF LN-SSNO(SUB) NOT = FIND-SSNO
              GO TO TEST-SSNO-EXIT.

           IF ( LN-POCD-NONLOAN )
              GO TO SSNO-READ-NEXT.

      * DONT WANT ANY OPEN LOANS !!
      * ONLY WANT PB'D LOANS FROM TODAY !!

           IF LN-POFFDATE NOT = 0
              IF LN-POFFDATE NOT = FIND-TRANS-DATE
                 GO TO SSNO-READ-NEXT.

      * ONLY FIND LOANS WITH SAME STATUS, GIFTNET OR NOT GIFTNET
      * OLD LOANS DIDNT GET GIFTNET FLAG NET TO "N", SO ALLOW A " " AND "N"
      * TO BE A MATCH
           IF LN-GIFTNET NOT = FIND-GIFTNET
              IF LN-GIFTNET = " " AND FIND-GIFTNET = "N"
                 NEXT SENTENCE
              ELSE
                 GO TO SSNO-READ-NEXT.

           MOVE LN-ACCTNO TO FOUND-NUMBER(LN-COUNT).
           IF LN-COUNT < 5
              ADD 1 TO LN-COUNT
           ELSE
              GO TO TEST-SSNO-EXIT.

           GO TO SSNO-READ-NEXT.

       TEST-SSNO-EXIT.
           PERFORM CLOSE-LN4-FILE.

      **********************************************
      *    START LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       START-LN-FILE SECTION.
           IF SUB = 1
              MOVE LN-PATH-OWNBR TO QLN4-WBEG-OWNBR
                                    QLN4-WEND-OWNBR
              MOVE LN-SSNO(1)    TO QLN4-WBEG-SSNO1
                                    QLN4-WEND-SSNO1
              PERFORM START-LN4-FILE
           ELSE
              IF SUB = 2
                 MOVE LN-PATH-OWNBR TO QLN5-WBEG-OWNBR
                                       QLN5-WEND-OWNBR
                 MOVE LN-SSNO(2)    TO QLN5-WBEG-SSNO2
                                       QLN5-WEND-SSNO2
                 PERFORM START-LN5-FILE.
      *       ELSE
      *          IF SUB = 3
      *             MOVE LN-PATH-OWNBR TO QLN6-WBEG-OWNBR
      *                                   QLN6-WEND-OWNBR
      *             MOVE LN-SSNO(3)    TO QLN6-WBEG-SSNO3
      *                                   QLN6-WEND-SSNO3
      *             PERFORM START-LN6-FILE
      *          ELSE
      *             IF SUB = 4
      *                MOVE LN-PATH-OWNBR TO QLN7-WBEG-OWNBR
      *                                      QLN7-WEND-OWNBR
      *                MOVE LN-SSNO(4)    TO QLN7-WBEG-SSNO4
      *                                      QLN7-WEND-SSNO4
      *                PERFORM START-LN7-FILE.

      **********************************************
      *    READ-NEXT LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       READ-LN-FILE-NEXT SECTION.
           IF SUB = 1
              PERFORM READ-LN4-FILE-NEXT
           ELSE
              IF SUB = 2
                 PERFORM READ-LN5-FILE-NEXT.
      *       ELSE
      *          IF SUB = 3
      *             PERFORM READ-LN6-FILE-NEXT
      *          ELSE
      *             IF SUB = 4
      *                PERFORM READ-LN7-FILE-NEXT.


       COPY "LIBGB/DATER.CPY".

      *********************************
       MISC-ROUTINES SECTION.
      *********************************
       FORM-RESET.
       VDU-CONVERT-FIELD.
       SETUP-LINK-INFO.

      *********************************
      *      F I L E   I / O          *
      *********************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE SECTION.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BICREA.
       DATE-WRITTEN.    072591.
      ********************************************************************
      *   DIRECTORY  : CL
      *
      *   DESCRIPTION: CALLED PROGRAM TO CREATE / MODIFY / DELETE
      *                BIFILE MASTER INDEX RECORDS
      *
      *   OUTPUT:
      *     IF AN ERROR OCCURS THE
      *
      * REV:
      *  JTG 092496 CHANGED ALL READS ON BIFILE TO UNLOCK THE RECORD
      *             WHEN IO-FG = 9
      *  BAH 081497 CHANGED BI-SALEFIN TO BI-GIFTNET AND BI-LN-SALEFIN
      *             TO BI-LN-GIFTNET
      *  MJD 122997 ADDED GP-SKIP-BI-UPDATE
      *  BLV 020698 ADDED BACK BI-SALEFIN & BI-LN-SALEFIN
      *  MJD 030398 ADDED BI-PLCD AND BI-CONVERCD
      *  BAH 060898 ADDED REFRESH OF GP ENV
      *  MG  990927  FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLV 011109 ADDED LOGIC FOR MERCURY'S AUTO LOAN CREATE TO GET
      *             THE CORRECT BORROWER NAME; LP/LNCREA CALLS LONPB2
      *             WHICH CALLS THIS PROGRAM; NOW IF CALLED FROM LNCREA
      *             USE BICREA-LN-OWNBR TO READ LOAN'S BRANCH & SET UP
      *             BW-ALL-PATH BEFORE READING BW
      *  BAH 040225 UPDATE BI-PLCD ON REWRITE
      *  BAH 040301 ADDED BI-STAT = "F", FIND AND FIX A BORROWER'S NAME
      *             ONLY FOR THAT OWNING BRANCH...
      *  BAH 050201 NEED BULKG2 TO ALSO USE BICREA-LN-OWNBR TO READ LOANS
      *             BRANCH AND SETUP BW-ALL-PATH BEFORE READING BW
      *             NEW BI'S GETTING INVALID NAMES
      *  BAH 061005 ADDED STAT "CK" TO CREATE ONE BI RECORD FOR A GIVEN
      *             SSNO IN THE BI-CHTO-SSNO, CALLED IN WI/COSIGN
      *   CS 080606 NEED BULKT2 TO ALSO USE BICREA-LN-OWNBR & SETUP
      *             BW-ALL-PATH, BORROWER NAME 'NOT ON FILE' ADDED BULKT2
      *             TO BETHS CHANGE MADE 2/01/05 PR#631 RIVERSIDE
      *  BAH 130201 A30 CONVERSION
      *  BAH 160422 ADDED BI-SMALLOAN FOR DUAL TEST, REGENCY PR#5030
      *  MJD 160303 ADDED LNCRE2 TO BICREA-CALLED-FROM. WORLD PR#1133
      *  BAH 160422 ADDED BI-SMALLOAN FOR DUAL TEST, REGENCY PR#5030
      *  BAH 180316 ADDED BI-PLDATE FOR DUAL TEST, WORLD PR#1308
      *  BAH 180327 NEED CLAFSB TO ALSO USE BICREA-LN-OWNBR TO READ
      *             LOANS BRANCH AND SETUP BW-ALL-PATH BEFORE READING
      *             BW, NEW BI'S GETTING INVALID NAMES, WORLD #1310
      *  BAH 220629 ADDED AN ENVIRONMENT VARIABLE THAT YOU CAN SET IN CRON
      *             SHELL SCRIPTS SO THE SEND-MESS WONT HANG WAITING FOR
      *             PRESS RETURN; ADD "EXPORT CRONFG=Y" TO YOUR CRON SHELL
      *             WAS HANGING DURING BATCH "PL" POSTING, BIFILE NOT UP
      *             CURRENT. CREATE ENTRY IN ERR.YYMMDD             #1548
      *  BAH 20220830 HARD CODED START
      ********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./CL/BICREA.CBL S35 09/20/22-12:29:54 >".
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  ERRCD               PIC X        VALUE SPACES.
       01  SUB                 PIC 999 COMP VALUE 0.
       01  SUB-DEL             PIC 999 COMP VALUE 0.

      * ADDED AN ENVIRONMENT VARIABLE THAT YOU CAN SET IN YOUR CRON SHELL
      * SCRIPTS SO THE SEND-MESS WONT HANG WAITING FOR YOU TO PRESS RETURN
      * ADD "EXPORT CRONFG=Y" TO YOUR CRON SHELL
       01  CRON-FLAG           PIC X     VALUE "N".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".

       01  BI-STAT                PIC XX.
       01  BI-LN-ACCTNO           PIC 9(8).
       01  BI-LN-BWTABLE.
           03  BI-LN-BORRTBL      OCCURS 4 TIMES.
               05  BI-LN-SSNO     PIC 9(9).
               05  BI-LN-MAKERCD  PIC X.
       01  BI-LN-OWNBR            PIC 9999.
       01  BI-LN-ORGST            PIC XX.
       01  BI-LN-SALEFIN          PIC X.
       01  BI-LN-GIFTNET          PIC X.
       01  BI-LN-ENTDATE          PIC 9(8)      COMP-3.
       01  BI-LN-POFFDATE         PIC 9(8)      COMP-3.
       01  BI-LN-PLCD             PIC X.
       01  BI-LN-CONVERCD         PIC X.
       01  BI-CHTO-SSNO           PIC 9(9).
       01  BI-LN-SMALLOAN         PIC X.
       01  BI-LN-PLDATE           PIC 9(8)      COMP-3.


       PROCEDURE DIVISION
            USING FORM-PATHNAME
                    BI-STAT
                     BI-LN-ACCTNO
                      BI-LN-BWTABLE
                       BI-LN-OWNBR
                        BI-LN-ORGST
                         BI-LN-SALEFIN
                          BI-LN-GIFTNET
                           BI-LN-ENTDATE
                            BI-LN-POFFDATE
                             BI-LN-PLCD
                              BI-LN-CONVERCD
                               BI-CHTO-SSNO
                                BI-LN-SMALLOAN
                                 BI-LN-PLDATE.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           IF NOT (BI-STAT = "W" OR "R" OR "D" OR "SS" OR "F" OR "W1")
              MOVE "E" TO BI-STAT
              GO TO END-ROUTINE.

      *  5/23/01 - THESE 2 LINES WERE ADDED FOR MERCURY - PURGING ON M2
      *  & DELETING BI'S ACROSS NFS LINE WAS TAKING FOREVER - AND WE
      *  KNEW WE NEEDED TO REBUILD BI ANYWAY.....
      *
      *    IF BI-STAT = "D"
      *       GO TO END-ROUTINE.

      * 01/11/2021 COMMENTED OUT THE SETENV CALL AND THE ACCEPT GP-REC, WE 
      * ADDED THIS WHEN MERCURY WAS HAVING ALL THEIR NETWORK ISSUES AND
      * /USR/DATA/SH/BIFILE WOULD BOMB OUT. THEY WOULD QUICKLY CHANGE THE
      * GP FLAG TO SKIP THE BI UPDATE AND WE WANTED THE BRANCHES TO CATCH
      * THE FLAG CHANGE WITHOUT INTERRUPTION.
      *
      * CINDY FOUND THIS WHEN ADDING LIVE CHECKS TO A32/A34, SETENV FORCES
      * ROUTE BUT TO 00 AND THEN RUNNING IT IN BATCH CAUSES MESSAGES TO THE
      * SCREEN. ALL THIS WAS UNNECESSARY CODE.

      * THIS CALL TO SETENV IS DONE IN GB/TCLP.C, AND DONE ON F10 INTERRUPT
      * TO PROVIDE A MEANS OF REFRESHING THE ENVIRONMENT.
      *    MOVE "GB/SETENV" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH.
      *    CANCEL FORM-PROGX.

      *    ACCEPT GP-REC             FROM ENVIRONMENT "GPENV".

      * SET GP-SKIP-BI-UPDATE TO "Y" FOR MERC1 DOWN, MERC2 UP
           PERFORM GET-GPENV.
           IF GP-SKIP-BI-UPDATE = "Y"
              MOVE " " TO BI-STAT
              GO TO END-ROUTINE.

           PERFORM OPEN-BI1-FILE.


      *--------------------------------------------------------------------
      *  11/8/01 - FOLLOWING LOGIC ADDED FOR MERCURY'S AUTO LOAN CREATE:
      *  LP/LNCREA CALLS LONPB2 WHICH WRITES LOAN & UPDATES BI....BUT
      *  BI'S WERE GETTING WRITTEN WITH "NOT ON FILE" IN BORROWER NAME
      *  BECAUSE THE LOCAL BWFILE WAS GETTING OPENED

      * BICREA-CALLED-FROM IS EXTERNAL IN CONAME.EXT
      *    IF BICREA-CALLED-FROM = "LNCREA" OR "BULKG2" OR
      *                             "SUNLA" OR "BULKT2" OR
      *                             "LNCRE2" OR "CLAFSB"
           IF BICREA-CALLED-FROM = "BULKG2" OR "BULKT2" OR "LNCRE2" 
              PERFORM OPEN-BR-FILE
              MOVE BI-LN-OWNBR TO BR-NO
              PERFORM READ-BR-FILE
              IF IO-GOOD
                 MOVE EXT-FILPATH-BASE TO FILPATH-BASE
                 MOVE BR-MACHINE       TO FILPATH-MACHINE
                 MOVE BR-DATA-PATH     TO FILPATH-DATA
                 MOVE "B" TO BW-PATH-OVERRIDE
              END-IF
              PERFORM CLOSE-BR-FILE.
      *--------------------------------------------------------------------

           IF BI-STAT = "W"
              MOVE " " TO BI-STAT
              PERFORM CREATE-BI-RECORD
                  VARYING SUB FROM 1 BY 1
                                 UNTIL SUB > 4
           ELSE
           IF BI-STAT = "D"
              MOVE " " TO BI-STAT
              MOVE 0 TO SUB-DEL
              PERFORM DELETE-BI-RECORD
                  VARYING SUB FROM 1 BY 1
                                 UNTIL SUB > 4
           ELSE
           IF BI-STAT = "R"
              MOVE " " TO BI-STAT
              PERFORM REWRITE-BI-RECORD
                  VARYING SUB FROM 1 BY 1
                                 UNTIL SUB > 4
           ELSE
           IF BI-STAT = "SS"
              MOVE " " TO BI-STAT
              PERFORM CHANGE-SS-BI-RECORD
           ELSE
      * USED IN WI/COSIGN TO ADD NEW COSIGNER/LOAN COMBO TO BI FILE
           IF BI-STAT = "W1"
              MOVE " " TO BI-STAT
              PERFORM CREATE-ONE-BORROWER
           ELSE
           IF BI-STAT = "F"
              MOVE " " TO BI-STAT
              PERFORM FIND-AND-FIX-BI-NAME.


           PERFORM CLOSE-BI1-FILE.
           GO TO END-ROUTINE.

      *************************************
      *    CREATE BI RECORD
      *************************************
       CREATE-BI-RECORD SECTION.
           IF BI-LN-SSNO(SUB) = 0
              GO TO CREATE-BI-RECORD-EXIT.

           PERFORM CLEAR-BI-FILE.

           MOVE BI-LN-SSNO(SUB) TO BI-SSNO.
           MOVE BI-LN-ACCTNO TO BI-ACCTNO.
           MOVE BI-LN-OWNBR TO BI-OWNBR.

           MOVE BI-LN-SSNO(SUB) TO BW-SSNO.
           PERFORM GETBW.
           MOVE BW-LNAME TO BI-LNAME.
           MOVE BW-FNAME TO BI-FNAME.

           MOVE BI-LN-MAKERCD(SUB) TO BI-MAKERCD.
           MOVE BI-LN-ORGST        TO BI-ORGST.
           MOVE BI-LN-SALEFIN      TO BI-SALEFIN.
           MOVE BI-LN-GIFTNET      TO BI-GIFTNET.
           MOVE BI-LN-ENTDATE      TO BI-ENTDATE.
           MOVE BI-LN-POFFDATE     TO BI-POFFDATE.

           MOVE BI-LN-PLCD     TO BI-PLCD.
           MOVE BI-LN-CONVERCD TO BI-CONVERCD.
           MOVE BI-LN-SMALLOAN TO BI-SMALLOAN.
           MOVE BI-LN-PLDATE   TO BI-PLDATE.

           PERFORM WRITE-BI1-FILE.
           IF IO-BAD
      *       UNLOCK BI-FILE
              MOVE "W" TO BI-STAT
              STRING
                "AN ERROR OCCURED CREATING BI-INDEX FOR SSNO: "
                      BI-LN-SSNO(SUB)
                           DELIMITED BY SIZE
                              INTO MESS
              PERFORM CONDITIONAL-SEND-MESS.

       CREATE-BI-RECORD-EXIT.
           EXIT.

      *************************************
      *    CREATE ONE BI RECORD
      *************************************
       CREATE-ONE-BORROWER SECTION.
           IF BI-CHTO-SSNO = 0
              GO TO CREATE-BI-RECORD-EXIT.

           PERFORM CLEAR-BI-FILE.

           MOVE BI-CHTO-SSNO TO BI-SSNO BW-SSNO.
           MOVE BI-LN-ACCTNO TO BI-ACCTNO.
           MOVE BI-LN-OWNBR TO BI-OWNBR.

           PERFORM GETBW.
           MOVE BW-LNAME TO BI-LNAME.
           MOVE BW-FNAME TO BI-FNAME.

           IF BI-CHTO-SSNO = BI-LN-SSNO(2)
              MOVE 2 TO SUB
           ELSE
           IF BI-CHTO-SSNO = BI-LN-SSNO(3)
              MOVE 3 TO SUB
           ELSE
              MOVE 4 TO SUB.
           MOVE BI-LN-MAKERCD(SUB) TO BI-MAKERCD.
           MOVE BI-LN-ORGST        TO BI-ORGST.
           MOVE BI-LN-SALEFIN      TO BI-SALEFIN.
           MOVE BI-LN-GIFTNET      TO BI-GIFTNET.
           MOVE BI-LN-ENTDATE      TO BI-ENTDATE.
           MOVE BI-LN-POFFDATE     TO BI-POFFDATE.

           MOVE BI-LN-PLCD     TO BI-PLCD.
           MOVE BI-LN-CONVERCD TO BI-CONVERCD.
           MOVE BI-LN-SMALLOAN TO BI-SMALLOAN.
           MOVE BI-LN-PLDATE   TO BI-PLDATE.

           PERFORM WRITE-BI1-FILE.

       CREATE-BI-RECORD-EXIT.
           EXIT.

      *************************************
      *    DELETE BI RECORD
      *************************************
       DELETE-BI-RECORD SECTION.
           IF BI-LN-SSNO(SUB) = 0
              GO TO DELETE-BI-RECORD-EXIT.

           MOVE BI-LN-SSNO(SUB) TO BI-SSNO.
           MOVE BI-LN-ACCTNO TO BI-ACCTNO.
           MOVE BI-LN-OWNBR TO BI-OWNBR.
           PERFORM READ-BI1-FILE.
           IF IO-BAD
      *       UNLOCK BI-FILE
              MOVE "D" TO BI-STAT
              GO TO DELETE-BI-RECORD-EXIT.

           MOVE SUB TO SUB-DEL.

           PERFORM DELETE-BI1-FILE.
           IF IO-BAD
      *       UNLOCK BI-FILE
              MOVE "D" TO BI-STAT
              STRING
                "AN ERROR OCCURED DELETING BI-INDEX FOR SSNO: "
                      BI-LN-SSNO(SUB)
                           DELIMITED BY SIZE
                              INTO MESS
              PERFORM CONDITIONAL-SEND-MESS.

       DELETE-BI-RECORD-EXIT.
           EXIT.

      *************************************
      *    REWRITE BI RECORD
      *************************************
       REWRITE-BI-RECORD SECTION.
           IF BI-LN-SSNO(SUB) = 0
              GO TO REWRITE-BI-RECORD-EXIT.

           MOVE BI-LN-SSNO(SUB) TO BI-SSNO.
           MOVE BI-LN-ACCTNO TO BI-ACCTNO.
           MOVE BI-LN-OWNBR TO BI-OWNBR.
           PERFORM READ-BI1-FILE.
           IF IO-BAD
      *       UNLOCK BI-FILE
              GO TO REWRITE-BI-RECORD-ERR.

           MOVE BI-LN-SSNO(SUB) TO BW-SSNO.
           PERFORM GETBW.
           MOVE BW-LNAME TO BI-LNAME.
           MOVE BW-FNAME TO BI-FNAME.

           MOVE BI-LN-MAKERCD(SUB) TO BI-MAKERCD.
           MOVE BI-LN-ORGST        TO BI-ORGST.
           MOVE BI-LN-SALEFIN      TO BI-SALEFIN.
           MOVE BI-LN-GIFTNET      TO BI-GIFTNET.
           MOVE BI-LN-ENTDATE      TO BI-ENTDATE.
           MOVE BI-LN-POFFDATE     TO BI-POFFDATE.
           MOVE BI-LN-PLCD         TO BI-PLCD.
           MOVE BI-LN-SMALLOAN     TO BI-SMALLOAN.
           MOVE BI-LN-PLDATE       TO BI-PLDATE.

           PERFORM REWRITE-BI1-FILE.
           IF IO-GOOD
              GO TO REWRITE-BI-RECORD-EXIT.
      *    UNLOCK BI-FILE.

       REWRITE-BI-RECORD-ERR.
           MOVE "R" TO BI-STAT
           STRING
             "AN ERROR OCCURED REWRITING BI-INDEX FOR SSNO: "
                   BI-LN-SSNO(SUB)
                        DELIMITED BY SIZE
                           INTO MESS
           PERFORM CONDITIONAL-SEND-MESS.

       REWRITE-BI-RECORD-EXIT.
           EXIT.

      *************************************
      *    CHANGE BORROWER SSNO BI RECORD
      *************************************
       CHANGE-SS-BI-RECORD SECTION.
           IF BI-CHTO-SSNO = 0
              GO TO CHANGE-SS-BI-RECORD-ERR.

           MOVE 0 TO SUB-DEL.
           PERFORM DELETE-BI-RECORD
               VARYING SUB FROM 1 BY 1
                              UNTIL SUB > 4.
           IF BI-STAT NOT = " "
              GO TO CHANGE-SS-BI-RECORD-ERR.

           MOVE 0 TO BI-LN-SSNO(1)
                     BI-LN-SSNO(2)
                     BI-LN-SSNO(3)
                     BI-LN-SSNO(4).
           MOVE BI-CHTO-SSNO TO BI-LN-SSNO(SUB-DEL).
           PERFORM CREATE-BI-RECORD
               VARYING SUB FROM 1 BY 1
                              UNTIL SUB > 4.
           IF BI-STAT = " "
              GO TO CHANGE-SS-BI-RECORD-EXIT.

       CHANGE-SS-BI-RECORD-ERR.
           MOVE "S" TO BI-STAT.
           MOVE "AN ERROR OCCURED CHANGING BI-INDEX " TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.

       CHANGE-SS-BI-RECORD-EXIT.
           EXIT.

      *************************************
      *    CHANGE BORROWER NAME BI RECORD
      * ONLY HERE IF NAME WAS CHANGED IN CHBORR PROGRAM
      *************************************
       FIND-AND-FIX-BI-NAME SECTION.

      * WILL ONLY HAVE 1 BI-LN-SSNO....BORROWER YOU JUST CHANGED
           IF BI-LN-SSNO(1) = 0
              GO TO FIND-AND-FIX-EXIT.

           MOVE BI-LN-SSNO(1) TO BI-SSNO
                                 QBI1-WBEG-SSNO
                                 QBI1-WEND-SSNO.
           MOVE 0             TO BI-ACCTNO BI-OWNBR
                                 QBI1-WBEG-ACCTNO
                                 QBI1-WBEG-OWNBR.
           MOVE ALL "9"       TO QBI1-WEND-ACCTNO
                                 QBI1-WEND-OWNBR.

           PERFORM START-BI1-FILE.
       FIND-AND-FIX-NEXT.
           PERFORM READ-BI1-FILE-NEXT.
           IF IO-BAD
      *       UNLOCK BI-FILE
              GO TO FIND-AND-FIX-EXIT.

           IF BI-SSNO NOT = BI-LN-SSNO(1)
      *       UNLOCK BI-FILE
              GO TO FIND-AND-FIX-EXIT.

           IF BI-OWNBR NOT = BI-LN-OWNBR
      *       UNLOCK BI-FILE
              GO TO FIND-AND-FIX-NEXT.

           MOVE BI-LN-SSNO(1) TO BW-SSNO.
           PERFORM GETBW.
           MOVE BW-LNAME TO BI-LNAME.
           MOVE BW-FNAME TO BI-FNAME.

           PERFORM REWRITE-BI1-FILE.
      *    UNLOCK BI-FILE.

           GO TO FIND-AND-FIX-NEXT.


       FIND-AND-FIX-EXIT.
           EXIT.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPBICL.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************
      *    MISC SECTION
      **************************************
       MISC SECTION.
       COPY "LIBGB/GETENV.CPY".
       CONDITIONAL-SEND-MESS.
      * DO NOT PERFORM SEND-MESS IF YOU ARE RUNNING FROM CRON, IT HANGS WAITING
      * FOR A PRESS RETURN!
           IF CRON-FLAG NOT = "Y"
              PERFORM SEND-MESS
           ELSE
              MOVE "E"           TO ERRLOGW-ACTION
              MOVE E-MSG         TO ERRLOGW-TYPE
              MOVE E-CODE        TO ERRLOGW-CODE
              MOVE E-SUBCODE     TO ERRLOGW-SUBCODE
              MOVE BI-PATH       TO ERRLOGW-FILE
              MOVE FORM-PATHNAME TO ERRLOGW-PROGX
              MOVE BI1-KEY       TO ERRLOGW-KEY
              MOVE "CL/ERRLOG"   TO FORM-NAM
              CALL FORM-PATH USING FORM-PATH ERRLOGW-AREA EXIT-PATHNAME
              CANCEL FORM-PATH.
       GETBW.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-BAD
              MOVE "**NOT ON FILE**" TO BW-LNAME
              MOVE " " TO BW-FNAME.


      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BI1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPBI1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO BI-STAT.

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.

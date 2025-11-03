       IDENTIFICATION DIVISION.
       PROGRAM-ID.   VCQUPD.
       AUTHOR.       JKC.
       DATE-WRITTEN. 04-22-2025.
      ******************************************************************
      *
      * ****NOTE****  : THE VCQFILE ****NEEDS**** TO BE CLOSED PRIOR
      *                 TO CALL THIS PROGRAM.  THIS PROGRAM NEEDS TO
      *                 DO WRITES AND/OR REWRITES WHICH CAUSES
      *                 LOCKING ISSUES!!!
      *
      * IDENTIFICATION: CL/VCQUPD.CBL
      *
      * DESCRIPTION   : VCQFILE UPDATE PROGRAM
      *
      *                 TEMPORARY VISION FILE CREATION - VCQFILE
      *                 VCQFILE IS TO RESOLVE DIFFICULTY WITH THE 
      *                 CQ2-KEY WORKING WITH A SCAN WINDOW AND/OR
      *                 USING A START-GREATER IO PARAGRAPH.  USING A
      *                 TEMPORARY VISION CQFILE HELPS KEEP THE SAME
      *                 EXPECT RESULTS FROM A15.
      *
      *=================================================================
      * THERE ARE TWO PURPOSES FOR THE VCQFILE:
      *
      *  1. FOR SCAN PAGE FUNCTIONALITY THAT CANNOT BE ACHIEVED FOR THE
      *     CQ2-KEY IN SQL BUT CAN BE DONE IN VISION.  DUE TO THE 
      *     COMPLEXITY OF THE CQ2-KEY, IT IS HARD TO NAVIGATE WITH A
      *     PAGE UP & PAGE DOWN WITHIN A SCAN WINDOW (WI/CQSCAN)
      *
      *  2. FOR DEALING WITH THE COLLECTION WORK SCREEN (SPINQ & XPINQ)
      *     WORKING WITH THE CQ2-KEY AND ITS DUPLICATES.  THIS IS
      *     BECAUSE IT REQUIRES A START-GREATER FUNCTION. DUE TO THE
      *     COMPLEXITY OF THE CQ2-KEY, GETTING A PROPER SQL WHERE
      *     CONSTRAINT TO WORK IN A START-GREATER USAGE IS VERY
      *     DIFFICULT.  IT IS EASIER TO USE THE VCQFILE.
      *
      * WITHIN THE CQ2-KEY, CQ-SEQ WAS ADDED BY BARB BACK IN A30 IN
      * ATTEMPT TO WORK WITH THE CQ2-KEY DUPLICATES.  CINDY ALSO TRIED
      * WORKING WITH CQ-SEQ FOR THE DUPLICATES WITHIN S35.  THEIR
      * EFFORTS DID NOT WORK AND NEVER FOUND A RESOLUTION.
      *
      * IN SEPTEMBER 2023, STEVE AND JAY WORKED ON CREATING THE VCQFILE
      * TO DEAL WITH THE NAVIGATION WITHIN THE SCAN WINDOW.  THIS
      * RESOLVED IT.
      *
      * IN MARCH 2025, BETH AND JAY WORKED ON THE ISSUES THAT S35
      * USER TESTING WAS GETTING (LOCKING, DISPLAY OUT OF ORDER,
      * NOT GETTING THE NEXT ACCOUNT WHEN SKIPPING).  THEY FOUND
      * CQ-SEQ WAS ALWAYS ZEROES AND NO LOGIC THAT WOULD INCREMENT IT.
      * NOW IN THE COLLECTION WORK SCREEN (SPINQ & XPINQ; LIBSP/SPINQ2),
      * AT THE START, IT WILL LOAD ALL CQ-REC FOR THAT GIVEN BRANCH
      * INTO THE USER'S VCQFILE THEN IT WILL GO THROUGH IT USING
      * VCQ2-KEY LOOKING FOR DUPLICATES AND ADDING ONE TO VCQ-SEQ IF IT
      * IS A DUPLICATE (CL/VCQUPD).  FURTHER INTO THE PROGRAM, IT
      * WILL USE MOSTLY THE VCQFILE BUT AT TIMES WILL STILL READ THE
      * CQFILE (SQL) FOR THE MATCHING RECORD (VCQ-CQ-REC) TO ENSURE IT
      * WAS NOT WORKED (CQ-WORKED-FG = "Y") OR IS CURRENTLY BEING WORKED
      * BY ANOTHER USER (CQ-IN-PROCESS = "Y") SINCE LOADED INTO THE
      * VCQFILE.  THE REWRITE-CQ1-VCQ1-FILE SECTION (LIBSP/SPINQ2) WAS
      * CREATED TO UPDATE THE CQ-REC AND VCQ-REC FOR CHANGES MADE.
      * THIS SECTION WILL:
      *
      *  1. MOVE CQ-REC TO HOLD-CQ-REC
      *  2. MOVE ZEROES TO CQ-SEQ
      *  3. READ CQFILE TO GET CQ-REC
      *  4. MOVE VCQ-SEQ INTO CQ-SEQ OF RECENT READ CQ-REC
      *  5. CHECK IF CQ-REC (SQL) EQUALS VCQ-CQ-REC
      *     A. IF EQUAL, MOVE HOLD-CQ-REC BACK TO CQ-REC
      *     B. IF NOT EQUAL, DISPLAY MESSAGE & FORCE EXIT OF PROGRAM
      *  6. REWRITE CQ-REC AND VCQ-REC
      *
      * THIS SECTION NEEDS TO ENSURE THAT THE GIVEN CQ-REC HAS NOT BEEN
      * ALTERED BY ANOTHER USER WHILE THE GIVEN USER WAS WORKING IT.
      * IF THE CQ-REC HAD BEEN ALTERED, WE NEED TO MESSAGE THE CURRENT
      * USER THAT A CHANGE HAD BEEN MADE AND NEED RESTART THE COLLECTION
      * WORK SCREEN IN ORDER TO RELOAD THE VCQFILE TO GET IT RESYNC WITH
      * THE LATEST DATA.  IF CQ-REC UPDATE WAS SUCCESSFUL THEN IT WILL
      * USE UPDATE-VCQ1-FILE-VCQUPD (LIBSP/SPVCQ1IN) TO CALL CL/VCQUPD
      * TO UPDATE THE VCQFILE AND RE-SEQUENCE THE VCQ2-KEY FOR
      * DUPLICATES IF FOUND.
      *
      *=================================================================
      * REV:
      * JKC 2025-0424 S35Q-200 ***NEW***
      *          ADDED NEW LOGIC FOR UPDATE THE VCQFILE;
      *          THE ORIGINAL VCQFILE WAS CREATED ONE PER USER PUT INTO
      *          /USR/TMP; THE NEW VCQFILE IS CREATED FOR ONE PER BRANCH
      *          PUT INTO /USR/DATA/R#/WK/VCQ#### WHICH WAS DONE FOR
      *          POTENTIALLY AVOID ISSUES WITH A VCQFILE ONE PER USER.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "LIBSP/SPFSVCQ.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBSP/SPFDVCQ.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/VCQUPD.CBL J35 04/24/25-14:42:19 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBSP/SP01CQ.CPY".
       COPY "LIBSP/SP01CQ_SQL.CPY".
       COPY "LIBSP/SPWSCQ.CPY".
       COPY "LIBSP/SPWSVCQ.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ERRCD                       PIC X(1)      VALUE SPACES.
       01  EXIT-PATHNAME               PIC X(22)     VALUE SPACES.

       01  FORM-PATHNAME               PIC X(22)     VALUE
                                           "/USR/S35/CO/CL/VCQUPD".
       01  FORM-PATH                   REDEFINES FORM-PATHNAME.
           03  FILLER                  PIC X(9).
           03  FORM-OBJ                PIC XX.
           03  FILLER                  PIC X.
           03  FORM-NAM.
               05  FILLER              PIC X(3).
               05  FORM-PROGX          PIC X(6).
           03  FILLER                  PIC X.

      *-----------------------------------------------------------------
       01  DUP-VCQ-WORKERS.
           03  DUP-VCQ1-KEY.
               05  DUP-VCQ-BRNO                      PIC 9(4).
               05  DUP-VCQ-ACCTNO                    PIC 9(8)    COMP-3.

           03  DUP-VCQ2-FIRST-TIME                   PIC X(1).

           03  DUP-VCQ2-KEY.
               05  DUP-VCQ-COLLID                    PIC XXX.
               05  DUP-VCQ-REV-BEHAVIOR-RISK         PIC 9.
               05  DUP-VCQ-SEQ-FG                    PIC X.
               05  FILLER                            PIC X.
               05  DUP-VCQ-DATE                      PIC 9(8)    COMP-3.
               05  DUP-VCQ-START-TIME                PIC S9(4)   COMP-3.
               05  DUP-VCQ-SEQ-FIELDS.
                   07  DUP-VCQ-REV-DEL               PIC 9(5).
                   07  DUP-VCQ-MISC-KEY-FLD          PIC X(3).
                   07  DUP-VCQ-REV-PDUE-DAYS REDEFINES
                       DUP-VCQ-MISC-KEY-FLD          PIC 9(5)    COMP-3.
                   07  DUP-VCQ-REV-BAL               PIC 9(7).
                   07  DUP-VCQ-SEQ                   PIC 9(3).

       01  UPDATE-VCQ-ACCTNO                         PIC 9(8).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

      ******************************************************************
      *
      *    L I N K A G E - S E C T I O N
      *
      ******************************************************************
       LINKAGE SECTION.
       COPY "LIBCL/VCQUPD_LNK.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING VCQUPD-LINK-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 VCQ-FILE.
       COPY "LIBGB/DECLARE.CPY".
           CLOSE VCQ-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-CQ1-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP VCQUPD-ACTION.

           MOVE VCQUPD-BRNO                TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO CQ-PATH-OVERRIDE.

           MOVE EXT-FILPATH-BASE           TO VCQ-ALL-BASE.
           MOVE FILPATH-MACHINE            TO VCQ-ALL-MACHINE.
           MOVE FILPATH-BRANCH             TO VCQ-ALL-BRNO.
           MOVE VCQ-ALL-PATH               TO VCQ-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VCQUPD-ACTION = "C" )
              PERFORM CREATE-VCQ-FILE
           ELSE
              PERFORM UPDATE-VCQ-FILE.

      *=================================================================
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *   C R E A T E - V C Q - F I L E
      *
      *=================================================================
      * IN  : FILPATH-WORKERS
      * OUT : VCQFILE                                    
      * DESC: THIS LOGIC LOOPS THRU THE CQ-FILE AND WRITES TO THE
      *       VCQ-FILE.
      * NOTE: ONLY CALLED FROM SP/CPRUN AND SP/CQRUN WHICH HAS SET
      *       THE FILPATH-WORKERS.
      ******************************************************************
       CREATE-VCQ-FILE SECTION.

           PERFORM OPEN-VCQ1-FILE-OUTPUT.
           PERFORM CLOSE-VCQ1-FILE.
           PERFORM OPEN-VCQ1-FILE.

           PERFORM OPEN-CQ1-FILE.

           MOVE BR-NO  TO CQ-BRNO.
           MOVE ZEROES TO CQ-ACCTNO.
 
           PERFORM START-CQ1-FILE.

      *=================================================================
       CREATE-VCQ-NEXT-CQ1.

           PERFORM READ-CQ1-FILE-NEXT.

           IF ( IO-BAD ) OR ( CQ-BRNO NOT = CQ-PATH-OWNBR )
              GO TO CREATE-VCQ-FILE-END.

           MOVE SPACES                     TO VCQ-REC.
           MOVE CQ-ACCTNO                  TO VCQ-ACCTNO.
           MOVE CQ-BRNO                    TO VCQ-BRNO.
           PERFORM LOAD-VCQ2-KEY.
           MOVE ZEROES                     TO VCQ-SEQ.
           MOVE CQ-REC                     TO VCQ-CQ-REC.
             
           PERFORM WRITE-VCQ1-FILE.

           GO TO CREATE-VCQ-NEXT-CQ1.

      *=================================================================
       CREATE-VCQ-FILE-END.

           MOVE ZEROES TO UPDATE-VCQ-ACCTNO.
           PERFORM SET-VCQ2-KEY-DUPLICATES.

           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-VCQ1-FILE. 

           MOVE ZEROES TO VCQUPD-IO-FG.

      *-----------------------------------------------------------------
       CREATE-VCQ-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T - V C Q 2 - K E Y - D U P L I C A T E
      *
      *=================================================================
      * IN  : UPDATE-VCQ-ACCTNO
      * OUT : UPDATE-VCQ-ACCTNO
      *       VCQFILE
      * DESC: SET VCQ-SEQ IF DUPLICATE VCQ2-KEY FOUND.
      *       FIX CQ2-KEY FOR DUPLICATES; ADD 1 TO CQ-SEQ IF DUPS.
      *
      *       UPDATE-VCQ-ACCTNO IS USED TO KNOW IF NEED TO FIX A
      *       SPECIFIC VCQ2-KEY (REWRITE/WRITE) OR FOR THE ENTIRE
      *       VCQ-FILE (CREATE).
      ******************************************************************
       SET-VCQ2-KEY-DUPLICATES SECTION.

           MOVE ZEROES TO VCQ-BRNO
                          VCQ-ACCTNO.

           MOVE SPACES TO VCQ2-KEY.
           MOVE ZEROES TO VCQ-REV-BEHAVIOR-RISK
                          VCQ-DATE
                          VCQ-START-TIME
                          VCQ-REV-DEL
                          VCQ-REV-PDUE-DAYS
                          VCQ-REV-BAL
                          VCQ-SEQ.

           IF ( UPDATE-VCQ-ACCTNO NOT = ZEROES )
              MOVE SPACES                  TO VCQ2-KEY
              MOVE CQ-COLLID               TO VCQ-COLLID
              MOVE CQ-REV-BEHAVIOR-RISK    TO VCQ-REV-BEHAVIOR-RISK
              MOVE CQ-SEQ-FG               TO VCQ-SEQ-FG
              MOVE CQ-DATE                 TO VCQ-DATE
              MOVE CQ-START-TIME           TO VCQ-START-TIME
              MOVE CQ-REV-DEL              TO VCQ-REV-DEL
              MOVE CQ-REV-PDUE-DAYS        TO VCQ-REV-PDUE-DAYS
              MOVE CQ-REV-BAL              TO VCQ-REV-BAL
      *KC     MOVE CQ-SEQ                  TO VCQ-SEQ.
              MOVE ZEROES                  TO VCQ-SEQ.

           MOVE "Y"      TO DUP-VCQ2-FIRST-TIME.
           MOVE VCQ1-KEY TO DUP-VCQ1-KEY.
           MOVE VCQ2-KEY TO DUP-VCQ2-KEY.
           PERFORM START-VCQ2-FILE.

      *=================================================================
       SET-VCQ2-KEY-DUPS-NEXT.

           PERFORM READ-VCQ2-FILE-NEXT.

           IF ( IO-BAD )
              GO TO SET-VCQ2-KEY-DUPS-END.

           IF ( VCQ1-KEY = DUP-VCQ1-KEY )
              GO TO SET-VCQ2-KEY-DUPS-NEXT.

           IF ( UPDATE-VCQ-ACCTNO NOT = ZEROES                     ) AND
              NOT
              (( VCQ-COLLID            = DUP-VCQ-COLLID            ) AND
               ( VCQ-REV-BEHAVIOR-RISK = DUP-VCQ-REV-BEHAVIOR-RISK ) AND
               ( VCQ-SEQ-FG            = DUP-VCQ-SEQ-FG            ) AND
               ( VCQ-DATE              = DUP-VCQ-DATE              ) AND
               ( VCQ-START-TIME        = DUP-VCQ-START-TIME        ) AND
               ( VCQ-REV-DEL           = DUP-VCQ-REV-DEL           ) AND
               ( VCQ-REV-PDUE-DAYS     = DUP-VCQ-REV-PDUE-DAYS     ) AND
               ( VCQ-REV-BAL           = DUP-VCQ-REV-BAL           ))
              GO TO SET-VCQ2-KEY-DUPS-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * ALREADY FIXED FOR A DUP WHICH VCQ-SEQ NOT = 0;
      * BY ADDING ONE TO VCQ-SEQ THEN REWRITE IT WILL CAUSE IT
      * RESEQUENCE ITSELF IN THE VISION INDEX WHICH CAN/WILL CAUSE
      * IT TO BE READ AGAIN IN THE READ-NEXT SO YOU NEED TO SKIP IT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF NOT
              (( VCQ-COLLID            = DUP-VCQ-COLLID            ) AND
               ( VCQ-REV-BEHAVIOR-RISK = DUP-VCQ-REV-BEHAVIOR-RISK ) AND
               ( VCQ-SEQ-FG            = DUP-VCQ-SEQ-FG            ) AND
               ( VCQ-DATE              = DUP-VCQ-DATE              ) AND
               ( VCQ-START-TIME        = DUP-VCQ-START-TIME        ) AND
               ( VCQ-REV-DEL           = DUP-VCQ-REV-DEL           ) AND
               ( VCQ-REV-PDUE-DAYS     = DUP-VCQ-REV-PDUE-DAYS     ) AND
               ( VCQ-REV-BAL           = DUP-VCQ-REV-BAL           ))
              MOVE VCQ1-KEY TO DUP-VCQ1-KEY
              MOVE VCQ2-KEY TO DUP-VCQ2-KEY
              MOVE "Y"      TO DUP-VCQ2-FIRST-TIME
              GO TO SET-VCQ2-KEY-DUPS-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * AT THIS POINT WE HAVE A DUPLICATE CQ2-KEY RECORD; NEED TO
      * INCREMENT THE VCQ-SEQ AND THE CQ-SEQ WITHIN VCQ-CQ-REC THEN
      * REWRITE THE RECORD. ON A REWRITE/WRITE (VCQ-ACCTNO NOT = ZEROES)
      * NEED TO FORCE THE THE FIRST DUPLICATE VCQ-SEQ TO BE ZEROES; ON 
      * A CREATE (VCQ-ACCTNO = ZEROES) THE THE CODE ABOVE WILL SKIP
      * THIS AREA ON THE FIRST DUPLICATE SINCE VCQ-SEQ WILL BE ZEROES.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE VCQ1-KEY    TO DUP-VCQ1-KEY.
           MOVE VCQ-CQ-REC  TO CQ-REC.

           IF ( UPDATE-VCQ-ACCTNO NOT = ZEROES ) AND
              ( DUP-VCQ2-FIRST-TIME   = "Y"    )
              MOVE "N"      TO DUP-VCQ2-FIRST-TIME
              MOVE ZEROES   TO DUP-VCQ-SEQ
           ELSE
              ADD  01       TO DUP-VCQ-SEQ.

           MOVE DUP-VCQ-SEQ TO VCQ-SEQ  CQ-SEQ.
           MOVE CQ-REC      TO VCQ-CQ-REC.
           PERFORM REWRITE-VCQ1-FILE.

           GO TO SET-VCQ2-KEY-DUPS-NEXT.

      *=================================================================
       SET-VCQ2-KEY-DUPS-END.

           MOVE ZEROES TO UPDATE-VCQ-ACCTNO.

       SET-VCQ2-KEY-DUPS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - V C Q - F I L E
      *
      *=================================================================
      * IN  : FILPATH-WORKERS
      *       VCQUPD-ACTION        [R=REWRITE; W=WRITE]
      *       VCQUPD-CQ-REC
      * OUT : VCQUPD-IO-FG
      *       VCQUPD-VCQ-REC
      *       VCQFILE
      * DESC: UPDATE VCQ-FILE WITH A NEW CQ-REC CREATED (WRITE-CQ1-FILE)
      ******************************************************************
       UPDATE-VCQ-FILE SECTION.

           MOVE VCQUPD-CQ-REC              TO CQ-REC.

           PERFORM OPEN-VCQ1-FILE.

           IF ( VCQUPD-ACTION = "R" )
              GO TO UPDATE-VCQ-FILE-REWRITE.

      *-----------------------------------------------------------------
       UPDATE-VCQ-FILE-WRITE.

           MOVE SPACES                     TO VCQ-REC
           MOVE CQ-ACCTNO                  TO VCQ-ACCTNO
           MOVE CQ-BRNO                    TO VCQ-BRNO
           PERFORM LOAD-VCQ2-KEY
           MOVE ZEROES                     TO VCQ-SEQ
           MOVE CQ-REC                     TO VCQ-CQ-REC.
             
           PERFORM WRITE-VCQ1-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO UPDATE-VCQ-FILE-END.

      *=================================================================
       UPDATE-VCQ-FILE-REWRITE.

           MOVE CQ-BRNO                    TO VCQ-BRNO
           MOVE CQ-ACCTNO                  TO VCQ-ACCTNO

           PERFORM READ-VCQ1-FILE.

           IF ( IO-BAD )
              GO TO UPDATE-VCQ-FILE-WRITE.
 
           PERFORM LOAD-VCQ2-KEY.
           MOVE CQ-REC                     TO VCQ-CQ-REC.
             
           PERFORM REWRITE-VCQ1-FILE.

      *=================================================================
       UPDATE-VCQ-FILE-END.

           MOVE IO-FG                      TO VCQUPD-IO-FG.

           MOVE 99999999                   TO UPDATE-VCQ-ACCTNO.
           PERFORM SET-VCQ2-KEY-DUPLICATES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO GET BACK THE VCQ-REC FOR CQ-ACCTNO AFTER DUP ROUTINE.
      *    NEED TO USE VCQUPD-CQ-REC TO GET THE ORIGINAL ACCTNO SO THAT
      *    WE CAN PULL BACK THE POSSIBLE CHANGED VCQ-REC AND CQ-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VCQUPD-CQ-REC              TO CQ-REC.
           MOVE CQ-BRNO                    TO VCQ-BRNO
           MOVE CQ-ACCTNO                  TO VCQ-ACCTNO
           PERFORM READ-VCQ1-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.
 
           PERFORM CLOSE-VCQ1-FILE.

       UPDATE-VCQ-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - R O U T I N E S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBSP/SPCQGS_SQL.CPY".

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBSP/SPCQ1RN.CPY".
       COPY "LIBSP/SPVCQ1IN.CPY".
       COPY "LIBSP/SPVCQ2RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
      *
      *    E X I T - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.

           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE SPACES     TO VCQUPD-ACTION.
           MOVE ZEROES     TO VCQUPD-BRNO.
           MOVE VCQ-CQ-REC TO VCQUPD-CQ-REC.
           MOVE VCQ-REC    TO VCQUPD-VCQ-REC.

       STOP-RUN.
           EXIT PROGRAM.

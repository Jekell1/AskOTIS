       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TR1534.
       DATE-WRITTEN.  08-27-2019.
      ******************************************************************
      *
      *  IDENTIFICATION: UP/TR1534.CBL
      *
      *  DESCRIPTION   : UPGRADE A15 TO A30 FOR TRANSACTION FILES
      *                  UPGRADE A15 TO A30 FOR CHANGES FILES
      *
      *    CH [LP01CH; LOCAL ]
      *    TR [LP01TR; LOCAL ]
      *
      * A15 GP-TR-RETAIN-MONTHS = 60 OR 5 YEARS
      *
      *=================================================================
      * REV:
      * BAH 20200423 CHANGED TO AUTOMATICALLY CREATE ASCII AND DONE DIRS
      * BAH 20200507 CLEANSE CH-DESCRIPT
      * BAH 20200508 REMOVED ATB-REFNO, MADE ATB-REF-FRTO AND ATB-REFBR
      *              ELEMENTARY ITEMS
      * BAH 20200511 MISSING SET OF TP-BRNO, TI-FROM-DATE AND TI-TO-DATE
      *              IN A15 ARE YYMMDD, NOT MMDDYY
      * JKC 2020-0817 ADD OPEN OUTPUT BEFORE ALL ASCII OPEN I-O. 
      *               A34 XCBLCONFIG HAS IO-CREATES 0, IT WONT CREATE A FILE
      *               WITH AN OPEN I/O, ONLY WITH AN OUTPUT.
      * BAH 20220524 ADDED ACTIONCD2 TO TRB, TRP, TRL, TRS #1551
      * BAH 20220921 ADDED TP-LCDUE #1570
      * JKC 20230515 HAD TO MAKE A CHANGE TO MOVE ZEROES TO CH-TYPE
      *         IF CH-CODE = "P" OTHERWISE MOVE OCH-TYPE TO CH-TYPE
      *         IF CH-CODE = "K"; THE UPGRADE DID NOT LIKE HAVING A
      *         "." IN CH-TYPE FROM CH-CODE = "P".
      *
      * BAH 20230516 FIXED OPEN OUTPUT OF ASCII FILES. HAVE TO DO BEFORE
      *              PROCESS STARTS SO THE GROUPS AND RANGES DONT WIPE IT OUT
      * BAH 20230519 SCRUB TP-LCDUE
      * BAH 20230728 HAD ATP-ORGTERM INSTEAD OF ATP-INSCOMM
      * JKC 20240130 CHANGED IN THE UPGRADE-FILE-CH FOR
      *         'IF ( OCH-CODE = "Y" )' TO BE 'IF ( OCH-CODE = "2" )';
      *         'IF ( OCH-CODE = "Q" OR "R" OR "T" ) TO BE 
      *         'IF ( OCH-CODE = "Q" OR "R" OR "T" )';
      *         MFB FOUND THIS WHILE CREATING UP/CHFILE.
      * BAH 20240131 REMOVED THE ALPHABETIC TEST ON OCH-DESCRIPT PER MARK
      *              NO IDEA WHY IT WAS DOING THIS!
      *
      * JKC 2024-0227 ADDED TRACE LOGIC
      * BAH 2024.0305 ADDED TP-REPAY-TRANS-ID #1641
      *
      * JKC 2024-0314 WITHIN CREATE-DONE-TAGS SECTION, NEED TO CHECK IF
      *               THE 'DONE-PATH-GROUP' IS NOT EQUAL SPACES; IT WILL
      *               BE EQUAL SPACES IF A GIVEN FILE'S SOURCE FILE WAS
      *               NOT FOUND;  IF THE SOURCE FILE IS FOUND THEN IT
      *               WILL POPULATE THE 'DONE-PATH-GROUP'; NEED THIS
      *               CHECK BECAUSE IT WILL STOP EXECUTION WITH A FILE
      *               IO-ERROR WINDOW.
      *
      * JKC 2024-0318 ADDED TO THE TRACE INITIALIZATION:
      *               + SQL DATABASE NAME
      *               + SQL SERVER NAME
      *               + BEGINNING DATE 
      *               + ENDING DATE 
      *
      * JKC 2024-0324 ADDED SYSTEM DATE TO THE "OTIS SESSION STARTED"
      *               LINE.
      *
      * JKC 2024-0325 CHANGED TO NOT MOVE THE A15 BRFILE TO BE OBRFILE;
      *     THIS WAS OLD ACU4GL LOGIC WHERE WE NEED TO MAKE IT OBRFILE;
      *     THIS OLD LOGIC CAUSED A LOCKING ISSUE WHEN RUNNING
      *     MULTIPLE SESSIONS VIA BATCHES (HAD 8 SESSIONS).
      *     NOW GOING TO USE DIRECTLY THE A15 BRFILE AS IS.
      * JKC 2024-0418 ADDED EXTRA CODE FOR BREAKDOWN OF ORG & NEW FOR
      *     OCH-CODE = "B"; IT WAS USING DATE FOR EVERYTHING WHEN IT 
      *     NEEDED TO INCLUDED DATE, AMT, NUMBER, TEXT.
      * JKC 2024-0424 ADDED SOME LOGIC TO SUPPRESS SEND MESS OF 
      *     ERROR MESSAGES FOR THE ASCII DIRECTORIES CREATION;
      *     ALSO ADDED LOGIC TO PUT THOSE MESSAGES INTO THE TRACE LOG.
      * BAH 2024-0718 ADDED DATABASE NAME AND SERVER NAME TO RANGE LINES
      *                                                         S35Q-102
      * JKC 2024-1108 ADDED USE SORT-RECS-SKIP-F-G; WITH OTR-CODE OF
      *     "F" OR "G" THEY ARE MERGED INTO A "P" RECORD; THE "F" OR
      *     "G" RECORDS WERE BEING COUNTED AS 'WRITE' WHICH IS
      *     MISLEADING WHEN LOOKING AT COUNTS IN THE ASCII FILES AND
      *     WHAT IS INSERTED INTO SQL; NOW WE CAN SEE HOW MANY RECORDS
      *     ARE MERGED.
      *
      * JKC 2024.1212 ADDED LOGIC TO REMOVE SORT WORKFILE.
      *
      * JKC 2024-1216 SKIPPING BRANCH 9991 DUE TO A15 BRFILE SAYING
      *     IT IS A 'R1' WHEN S35 HAS IT AS 'R2'; THERE IS NO DATA
      *     FOR 'R2' BUT THERE IS FOR 'R1' WHICH IS THROWING OFF
      *     COMPARISONS AND COUNTS.
      * BAH 2025.0116 ADDED OTP-CLOSFEE-REB-AMT #1697 S35Q-157
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      *=================================================================
      *    MSEXCEL EXTRACTION FILE
      *=================================================================
       COPY "LIBSP/SPFSSORT.CPY".
      *-----------------------------------------------------------------
           SELECT MSEXCEL-FILE ASSIGN OUTPUT MSEXCEL-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

      *=============================================[ SOURCE FILE - CH ]
           SELECT OCH-FILE ASSIGN TO OCH-PATH
                  ORGANIZATION SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

      *========================================[ SOURCE FILE - TR (LP) ]
           SELECT OTR-FILE ASSIGN TO OTR-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OTR1-KEY
                  FILE STATUS FILE-STAT.

      * ASCII UPLOAD FILES
       COPY "LIBUP/UPFSATRB.CPY".
       COPY "LIBUP/UPFSATRC.CPY".
       COPY "LIBUP/UPFSATRD.CPY".
       COPY "LIBUP/UPFSATRI.CPY".
       COPY "LIBUP/UPFSATRJ.CPY".
       COPY "LIBUP/UPFSATRK.CPY".
       COPY "LIBUP/UPFSATRL.CPY".
       COPY "LIBUP/UPFSATRP.CPY".
       COPY "LIBUP/UPFSATRS.CPY".
       COPY "LIBUP/UPFSATRW.CPY".
       COPY "LIBUP/UPFSATRX.CPY".
       COPY "LIBUP/UPFSACH.CPY".

      *=================================================================
           SELECT REPORT-FILE ASSIGN TO TR1534-REPORT-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY TR1534-REPORT-KEY
                  FILE STATUS FILE-STAT.

      * USING FOR ORIGINAL MACHINE
           SELECT OBR-FILE ASSIGN TO OBR-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OBR-KEY
                  FILE STATUS FILE-STAT.

      * USED FOR STORING "F" RECORDS, NEED TO KNOW IF WE WROTE A "P"
      * RECORD (FROM AN ORIGINAL "F" RECORD) BEFORE WE TRY TO WRITE IT AGAIN
      * ASCII FILE HAD NO WAY TO CHECK AS ITS RELATIVE/SEQUENTIAL
           SELECT WK-FILE ASSIGN WK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.

      ******************************************************************
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      * USING FOR BR-MACHINE IF FIX WAS ALREADY RUN
       FD  OBR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01BR.CPY" REPLACING LEADING "BR" BY "OBR".


      *************************************
      * USED FOR STORING "F" RECORDS, NEED TO KNOW IF WE WROTE A "P"
      * RECORD (FROM AN ORIGINAL "F" RECORD) BEFORE WE TRY TO WRITE IT AGAIN
      * ASCII FILE HAD NO WAY TO CHECK AS ITS RELATIVE/SEQUENTIAL
      * SKIPPING WRITING "F" RECORDS ALL TOGETHER IN THE NEW TRPFILE, ONLY
      * CREATING THIS WORK FILE FOR THE NEW TRP RECORD TO GET THE POOLID
      *************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WORK-BRNO        PIC 9(4).
               05  WORK-DATE        PIC 9(8).
               05  WORK-CLASS       PIC 9(2).
               05  WORK-ACCTNO      PIC 9(8).
               05  WORK-LINE        PIC 9(3).
           03  WORK-POOLID            PIC 9(5).

      *=================================================================
      *    SORT FILE FOR CREATING MSEXCEL EXTRACTION FILE
      *=================================================================
       COPY "LIBSP/SPFDSORT.CPY".
      *-----------------------------------------------------------------
       01  SORT-REC.
           03  SORT-KEY.
               05  SORT-FILENAME                     PIC X(08).
               05  SORT-FILEPATH                     PIC X(40).
               05  SORT-STATE                        PIC X(02).
               05  SORT-BRNO                         PIC 9(04).
           03  SORT-REC-INFO.
               05  SORT-RECS-READ                    PIC 9(15).
               05  SORT-RECS-WRITE                   PIC 9(15).
               05  SORT-RECS-SKIP                    PIC 9(15).
               05  SORT-RECS-SKIP-F-G                PIC 9(15).
               05  SORT-TIME-BEG                     PIC 9V9(7).
               05  SORT-TIME-END                     PIC 9V9(7).
               05  SORT-TIME-ELAPSED                 PIC 9V9(7).

      *=================================================================
      *    MSEXCEL EXTRACTION FILE
      *=================================================================
       FD  MSEXCEL-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 175 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  MSEXCEL-REC.
           03  MSEXCEL-BYTE             PIC X OCCURS 175 TIMES.

      *=============================================[ SOURCE FILE - CH ]
       FD  OCH-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/LP01CH.CPY" REPLACING LEADING "CH" BY "OCH".

      *========================================[ SOURCE FILE - TR (LP) ]
       FD  OTR-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       78  OTR-SIZE                     VALUE 550.
       01  OTR-REC.
           03  OTR1-KEY.
               05  OTR1-DAY              PIC 9(2)      COMP.
               05  OTR1-BRNO             PIC 9(4)      COMP.
               05  OTR1-CODE             PIC X.
                   88  OTR1-VALID-CD-B   VALUE "B".
                   88  OTR1-VALID-CD-C   VALUE "C".
                   88  OTR1-VALID-CD-D   VALUE "D".
                   88  OTR1-VALID-CD-G   VALUE "G".
                   88  OTR1-VALID-CD-I   VALUE "I".
                   88  OTR1-VALID-CD-J   VALUE "J".
                   88  OTR1-VALID-CD-K   VALUE "K".
                   88  OTR1-VALID-CD-L   VALUE "L".
                   88  OTR1-VALID-CD-P   VALUE "P".
                   88  OTR1-VALID-CD-S   VALUE "S".
                   88  OTR1-VALID-CD-X   VALUE "X".
                   88  OTR1-VALID-CD-ALL VALUE "B" "C" "D" "G" "J"
                                               "K" "L" "P" "S" "X" "I".
               05  OTR1-CLASS            PIC 99        COMP.
               05  OTR1-SUBKEY           PIC X(9).
           03  FILLER                   PIC X(536).
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/LPFDTRB.CPY" REPLACING LEADING "TB" BY "OTB".
       COPY "LIBUP/A15SRC/LPFDTRC.CPY" REPLACING LEADING "TC" BY "OTC".
       COPY "LIBUP/A15SRC/LPFDTRD.CPY" REPLACING LEADING "TD" BY "OTD".
       COPY "LIBUP/A15SRC/LPFDTRF.CPY" REPLACING LEADING "TF" BY "OTF".
      *COPY "LIBUP/A15SRC/LPFDTRG.CPY" REPLACING LEADING "TG" BY "OTG".
       COPY "LIBUP/A15SRC/LPFDTRI.CPY" REPLACING LEADING "TI" BY "OTI".
       COPY "LIBUP/A15SRC/LPFDTRJ.CPY" REPLACING LEADING "TJ" BY "OTJ".
       COPY "LIBUP/A15SRC/LPFDTRK.CPY" REPLACING LEADING "TK" BY "OTK".
       COPY "LIBUP/A15SRC/LPFDTRL.CPY" REPLACING LEADING "TL" BY "OTL".
       COPY "LIBUP/A15SRC/LPFDTRP.CPY" REPLACING LEADING "TP" BY "OTP".
       COPY "LIBUP/A15SRC/LPFDTRS.CPY" REPLACING LEADING "TS" BY "OTS".
       COPY "LIBUP/A15SRC/LPFDTRW.CPY" REPLACING LEADING "TW" BY "OTW".
       COPY "LIBUP/A15SRC/LPFDTRX.CPY" REPLACING LEADING "TX" BY "OTX".

      * ASCII UPLOAD FILES
       COPY "LIBUP/UPFDATRB.CPY".
       COPY "LIBUP/UPFDATRC.CPY".
       COPY "LIBUP/UPFDATRD.CPY".
       COPY "LIBUP/UPFDATRI.CPY".
       COPY "LIBUP/UPFDATRJ.CPY".
       COPY "LIBUP/UPFDATRK.CPY".
       COPY "LIBUP/UPFDATRL.CPY".
       COPY "LIBUP/UPFDATRP.CPY".
       COPY "LIBUP/UPFDATRS.CPY".
       COPY "LIBUP/UPFDATRW.CPY".
       COPY "LIBUP/UPFDATRX.CPY".
       COPY "LIBUP/UPFDACH.CPY".

      ******************************************************************
      *
      *    R E P O R T - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR PRINTING REPORT
      ******************************************************************
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD.

      *-----------------------------------------------------------------
       01  REPORT-REC.
           03  REPORT-DETAIL-LINE         PIC X(132).
           03  REPORT-LN-FEED             PIC 9(002).

      ******************************************************************
      ******************************************************************
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
      ******************************************************************
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UP/TR1534.CBL S35 12/16/24-11:33:01 >".

      ******************************************************************
      *
      *    F I L E - P A T H - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".

       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".

       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".

       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".

       COPY "LIBLP/LP01TRI.CPY".
       COPY "LIBLP/LP01TRI_SQL.CPY".
       COPY "LIBLP/LPWSTRI.CPY".

       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".

       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".

       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".

       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".

       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".

       COPY "LIBLP/LP01TRW.CPY".
       COPY "LIBLP/LP01TRW_SQL.CPY".
       COPY "LIBLP/LPWSTRW.CPY".

       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".

       COPY "LIBSP/SPWSSORT.CPY".

      *=================================================================
      *    EXAMPLE: /USR/TMP/TR1534-MSEXCEL-???????.TXT
      *=================================================================
       01  MSEXCEL-PATH                PIC X(35)     VALUE SPACES.
       01  FILLER                      REDEFINES MSEXCEL-PATH.
           03  MSEXCEL-PATH-SLASH-1     PIC X(01).    *> /
           03  MSEXCEL-PATH-DIR-1       PIC X(03).    *> USR
           03  MSEXCEL-PATH-SLASH-2     PIC X(01).    *> /
           03  MSEXCEL-PATH-DIR-2       PIC X(03).    *> TMP
           03  MSEXCEL-PATH-SLASH-3     PIC X(01).    *> /
           03  MSEXCEL-PATH-PREFIX      PIC X(15).    *> TR1534-MSEXCEL-
           03  MSEXCEL-PATH-SUFFIX      PIC X(07).    *> ???????
           03  MSEXCEL-PATH-EXT         PIC X(04).    *> .TXT

       01  WK-PATH                    PIC X(40)     VALUE SPACES.
      *-------------------------------------------------{ SOURCE PATHS }
       01  OCH-PATH                    PIC X(40)     VALUE SPACES.
       01  OTR-PATH                    PIC X(40)     VALUE SPACES.

      *-------------------------------------------------{ TARGET PATHS }

      * ASCII
       COPY "LIBUP/UPWSACH.CPY".
       COPY "LIBUP/UPWSATRB.CPY".
       COPY "LIBUP/UPWSATRC.CPY".
       COPY "LIBUP/UPWSATRD.CPY".
       COPY "LIBUP/UPWSATRI.CPY".
       COPY "LIBUP/UPWSATRJ.CPY".
       COPY "LIBUP/UPWSATRK.CPY".
       COPY "LIBUP/UPWSATRL.CPY".
       COPY "LIBUP/UPWSATRP.CPY".
       COPY "LIBUP/UPWSATRS.CPY".
       COPY "LIBUP/UPWSATRW.CPY".
       COPY "LIBUP/UPWSATRX.CPY".

      *-----------------------------------------------------------------
       01  WK-IFN                      PIC X(02)     VALUE "WK".
       01  TR1534-REPORT-IFN           PIC X(06)     VALUE "REPORT".
       01  TR1534-REPORT-KEY           PIC 9(09)     VALUE ZEROES.
       01  TR1534-REPORT-PATH          PIC X(35)     VALUE SPACES.

       01  ASCII-DIRECTORY-PATH.
           03  ASCII-DIR-BEG           PIC X(9).
           03  ASCII-FILLER            PIC X    VALUE "/".
           03  ASCII-DIR-MACHINE       PIC X(2).
           03  ASCII-DIR-NAME          PIC X(6) VALUE "/ASCII".
           03  ASCII-DIR-DONE          PIC X(5).

       01  HOLD-ASCII-PATH-GROUP      PIC X(4).
       01  HOLD-ASCII-PATH-BRNO    REDEFINES HOLD-ASCII-PATH-GROUP
                                      PIC 9(4).
      *-----------------------------------------------------------------
       01  PR-IFN                      PIC X(02)     VALUE "PR".
       01  OBR-KEY                     PIC 9(4).
       01  OBR-PATH                    PIC X(35)     VALUE SPACES.
      * ASCII FILES FOR SQL
       01  ATRB-KEY                    PIC 9(12) VALUE 0.
       01  ATRC-KEY                    PIC 9(12) VALUE 0.
       01  ATRD-KEY                    PIC 9(12) VALUE 0.
       01  ATRI-KEY                    PIC 9(12) VALUE 0.
       01  ATRJ-KEY                    PIC 9(12) VALUE 0.
       01  ATRK-KEY                    PIC 9(12) VALUE 0.
       01  ATRL-KEY                    PIC 9(12) VALUE 0.
       01  ATRP-KEY                    PIC 9(12) VALUE 0.
       01  ATRS-KEY                    PIC 9(12) VALUE 0.
       01  ATRW-KEY                    PIC 9(12) VALUE 0.
       01  ATRX-KEY                    PIC 9(12) VALUE 0.
       01  ACH-KEY                     PIC 9(12) VALUE 0.
       01  HOLD-CH-KEY                 PIC X(34) VALUE SPACES.
       01  HOLD-CH-SEQ                 PIC 9(3)  VALUE 0.

       01  TRFILE-MMYY                 PIC 9(04)     VALUE ZEROES.
       01  CHFILE-MMYY                 REDEFINES TRFILE-MMYY
                                       PIC 9(04).

       01  TRFILE-YYYYMM               PIC 9(06)     VALUE ZEROES.
       01  FILLER                      REDEFINES TRFILE-YYYYMM.
           03  TRFILE-YYYYMM-YYYY      PIC 9(04).
           03  TRFILE-YYYYMM-MM        PIC 9(02).
       01  CHFILE-YYYYMM               REDEFINES TRFILE-YYYYMM.
           03  CHFILE-YYYYMM-YYYY      PIC 9(04).
           03  CHFILE-YYYYMM-MM        PIC 9(02).

      *-----------------------------------------------------------------
       01  HOLD-SORT-KEY.
           03  HOLD-SORT-FILENAME      PIC X(08).
           03  HOLD-SORT-FILEPATH      PIC X(40).
           03  HOLD-SORT-STATE         PIC X(02).
           03  HOLD-SORT-BRNO          PIC 9(04).

       01  SAVE-SORT-KEY.
           03  SAVE-SORT-FILENAME      PIC X(08).
           03  SAVE-SORT-FILEPATH      PIC X(40).
           03  SAVE-SORT-STATE         PIC X(02).
           03  SAVE-SORT-BRNO          PIC 9(04).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  CASE-BUF                    PIC X(04)     VALUE SPACES.

      *- - - - - - - - - - - - - - - - - -{ EDIT DISPLAY FORMAT - EDF }
       01  EDF-WORKERS.
           03  EDF-SORT-TIME-BEG       PIC 9.9(7).
           03  EDF-SORT-TIME-END       PIC 9.9(7).
           03  EDF-SORT-TIME-ELAPSED   PIC 9.9(7).

       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  ERRCD-LIST                  PIC X(06)     VALUE "ERRCD.".

       01  EXITPROG                    PIC X(09)     VALUE "UP/UPMENU".
       01  EXITPROG-LIST               PIC X(09)     VALUE "EXITPROG.".

      *-----------------------------------------------[ HEX CHARACTERS ]
       01  HEX-CARRIAGE-RETURN         PIC X(01)     VALUE X"0D".
       01  HEX-LINE-FEED               PIC X(01)     VALUE X"0A".
       01  HEX-TAB                     PIC X(01)     VALUE X"09".

      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED                 PIC 9(03)     VALUE ZEROES.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.

       01  ER-FEED                     PIC 9(01)     VALUE 1.
       01  PAGE-NUMBER                 PIC 9(04)     VALUE ZEROES.

       01  BRANCH-TABLE.
           03  BRANCH-MACHINE          PIC XX OCCURS 9999 TIMES.

       01  SUBSCRIPT-WORKERS.
           03  ELE                     PIC 9(04)     VALUE ZEROES.
           03  SUB                     PIC 9(04)     VALUE ZEROES.
           03  TBSUB     REDEFINES SUB PIC 9(04).
           03  TJSUB     REDEFINES SUB PIC 9(03).
           03  TKSUB     REDEFINES SUB PIC 9(03).
           03  S1                      PIC 9(03)     VALUE ZEROES.
           03  SUB1      REDEFINES S1  PIC 9(03).
           03  S2                      PIC 9(03)     VALUE ZEROES.
           03  SUB2      REDEFINES S2  PIC 9(03).
           03  S3                      PIC 9(03)     VALUE ZEROES.
           03  SUB3      REDEFINES S3  PIC 9(03).

       01  TEST-TRCD                   PIC X(02).
           88  ADDON-TRCD              VALUE "RT" "LP" "NP" "ET" "ST".
           88  DEFER-TRCD              VALUE "DF" "D2" "D3" "D4" "D5" 
                                             "D6" "D7" "D8" "D9".
           88  PMT-TRCD                VALUE "PY" "PA".
           88  REBATE-TRCD             VALUE "RC" "RA" "RP" "R1" "R2"
                                             "R3" "RI" "RS" "RM" "RD"
                                             "CC" "CA" "CP" "C1" "C2"
                                             "C3" "CD".
           88  RESERVE-TRCD            VALUE "PR" "PC" "PP".
           88  RESERVE-P4P5-TRCD       VALUE "P4" "P5".
                                     
       01  TEST-REFNO                  PIC X(05)     VALUE SPACES.
       01  TEST-REFNO-TRANSFER         REDEFINES TEST-REFNO.
           03  TEST-FROMCL             PIC X(02).
           03  FILLER                  PIC X(01).
           03  TEST-TOCL               PIC X(02).

       01  TEST-ACCTNO                 PIC 9(8).
       01  FILLER REDEFINES TEST-ACCTNO.
             03  TEST-NUMBER           PIC 9(6).
             03  TEST-SEQ              PIC 9(2).    

      *=================================================[ UPGRADE-STAT ]
       78  UPGRADE-STAT---GOOD                     VALUE "00".
       78  UPGRADE-STAT---BAD                      VALUE "XX".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       77  UPGRADE-STAT                PIC X(02)   VALUE SPACES.
           88  UPGRADE-STAT-GOOD                   VALUE "00".
           88  UPGRADE-STAT-BAD                    VALUE "  " THRU "//"
                                                         "01" THRU "~~".

      *=================================================================
       01  WSS-CH-TIME                 PIC 9(06)   VALUE ZEROES.
       01  FILLER            REDEFINES WSS-CH-TIME.
           03  WSS-CH-HR               PIC 9(02).
           03  WSS-CH-MIN              PIC 9(02).
           03  WSS-CH-SEC              PIC 9(02).

       01  WSS-DATE-YYYYSMM            PIC 9999/99.

      *-----------------------------------------------------------------
       01  WSS-MSEXCEL-TIME-INPUT      PIC 9(6)      VALUE ZEROES.
       01  FILLER            REDEFINES WSS-MSEXCEL-TIME-INPUT.
           03  WSS-MSEXCEL-TIME-INPUT-HH   PIC 9(2).
           03  WSS-MSEXCEL-TIME-INPUT-MM   PIC 9(2).
           03  WSS-MSEXCEL-TIME-INPUT-SS   PIC 9(2).

       01  WSS-MSEXCEL-TIME-OUTPUT     PIC 9V9(7)    VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER PRINT".
           03  FILLER PIC X(30) VALUE "ER OPTIONS                    ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-SOURCE-PATH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER SOURC".
           03  FILLER PIC X(30) VALUE "E PATH                        ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-TARGET-PATH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER TARGET PATH               ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, UP/DOWN, ENTER BEGINNING".
           03  FILLER PIC X(19) VALUE " BRANCH NUMBER     ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER ENDING BRANCH NUMBER      ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEG-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER BEGINNING DATE (MMYY)     ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-END-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER ENDING DATE (MMYY)        ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-OPTION-FILES.
           03  FILLER PIC X(30) VALUE "F1 THRU F7, F8-HELP, UP/DOWN, ".
           03  FILLER PIC X(30) VALUE "ENTER (Y)ES/(N)O/(C)HECK FOR U".
           03  FILLER PIC X(19) VALUE "PDATING THIS FILE  ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-ACCEPT.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER TO ACCEPT (YES/NO)        ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEGIN-PROCESSING.
           03  FILLER PIC X(30) VALUE "PROCESSING....................".
           03  FILLER PIC X(30) VALUE "..............................".
           03  FILLER PIC X(19) VALUE "...................".

      ******************************************************************
      *
      *    M E S S A G E - W O R K E R S
      *
      ******************************************************************
       01  MESSAGE-INVALID-YN.
           03  FILLER PIC X(30) VALUE "INVALID: (Y)ES OR (N)O ONLY!!!".
           03  FILLER PIC X(30) VALUE "                              ".

       01  MESSAGE-INVALID-YNC.
           03  FILLER PIC X(30) VALUE "INVALID: (Y)ES OR (N)O OR (A)S".
           03  FILLER PIC X(30) VALUE "CII OR (C)HECK FILE ONLY!!!   ".

       01  MESSAGE-INVALID-FILE-PATH.
           03  FILLER PIC X(30) VALUE "INVALID FILE PATH; PATH NOT FO".
           03  FILLER PIC X(30) VALUE "UND!!!                        ".

       01  MESSAGE-INVALID-SAME-PATHS.
           03  FILLER PIC X(30) VALUE "INVALID: SOURCE AND TARGET CAN".
           03  FILLER PIC X(30) VALUE "NOT BE THE SAME               ".

      ******************************************************************
      *
      *    P R I N T - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-1.
           03  H1-DATE              PIC X(08)   VALUE SPACES.
           03  FILLER               PIC X(02)   VALUE SPACES.
           03  H1-TIME              PIC X(08)   VALUE SPACES.
           03  FILLER               PIC X(28)   VALUE SPACES.
           03  H1-NAME              PIC X(40)   VALUE SPACES.
           03  FILLER               PIC X(37)   VALUE SPACES.
           03  FILLER               PIC X(05)   VALUE "PAGE".
           03  H1-PAGE-NO           PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEADER-LINE-2.
           03  H2-USERID-PROMPT     PIC X(11)   VALUE "USER ID  : ".
           03  H2-USERID            PIC X(10)   VALUE SPACES.
           03  FILLER               PIC X(22)   VALUE SPACES.
           03  H2-TITLE             PIC X(46)   VALUE
               "----- TRANSACTIONS FILES  --- A15 TO A30 -----".
           03  FILLER               PIC X(43)   VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-RANGE-LINE        REDEFINES DETAIL-LINE.
           03  R1-DESC                        PIC X(34).
           03  FILLER                         PIC X(01).
           03  R1-FIELD                       PIC X(35).

           03  R1-DATE-RDF          REDEFINES R1-FIELD.
               05  R1-DATE                    PIC 99/99/99.
               05  FILLER                     PIC X(27).

           03  R1-MMYY-RDF          REDEFINES R1-FIELD.
               05  R1-MMYY                    PIC 99/99.
               05  FILLER                     PIC X(30).

           03  R1-NUMERIC-RDF       REDEFINES R1-FIELD.
               05  R1-NUMERIC                 PIC Z(09)9.
               05  FILLER                     PIC X(25).

           03  FILLER                         PIC X(62).

      ******************************************************************
      *
      *    T R 1 5 3 0 - W O R K E R S
      *
      ******************************************************************
       01  TR1534-RECORD-COUNTERS                    VALUE ZEROES.
           03  TR1534-RECORD-COUNTER-READ            PIC 9(10).
           03  TR1534-RECORD-COUNTER-WRITE           PIC 9(10).
           03  TR1534-RECORD-COUNTER-SKIP            PIC 9(10).
           03  TR1534-RECORD-COUNTER-SKIP-F-G        PIC 9(10).

       01  TR1534-RECORDS-FOUND        PIC X(01).
           88  TR1534-NO-RECORDS-FOUND               VALUE "N".
           88  TR1534-EXIT-PRIOR-TO-EXECUTION        VALUE "X".

       01  TR1534-UPDATE-KEY           PIC X(40)     VALUE SPACES.
       01  FILLER                      REDEFINES TR1534-UPDATE-KEY.
           03  TR1534-OCH-BRNO         PIC 9(04).
           03  TR1534-OCH-DAY          PIC 9(02).
           03  TR1534-OCH-CODE         PIC X(03).
           03  TR1534-OCH-USERID       PIC X(10).
           03  TR1534-OCH-TIME         PIC 9(06).
           03  TR1534-OCH-SEQ          PIC 9(02).
       01  FILLER                      REDEFINES TR1534-UPDATE-KEY.
           03  TR1534-OLPTR1-DAY       PIC 9(02).
           03  TR1534-OLPTR1-BRNO      PIC 9(04).
           03  TR1534-OLPTR1-CODE      PIC X(01).
           03  TR1534-OLPTR1-CLASS     PIC 9(02).
      *KC> 03  TR1534-OLPTR1-SUBKEY    PIC X(09).

       01  TR1534-UPDATE-STAT          PIC X(05)     VALUE SPACES.

       01  TR1534-UPDATE-TYPE          PIC X(01)     VALUE SPACES.
           88  TR1534-UPDATE-TYPE-ERROR              VALUE "E".
           88  TR1534-UPDATE-TYPE-SKIP               VALUE SPACES.

      ******************************************************************
      *
      *    T R 1 5 3 0 - F I L E - W O R K E R S
      *
      ******************************************************************
       01  TR1534-CURRENT-FILE                 PIC X(08)   VALUE SPACES.
       01  TR1534-CURRENT-MMYY                 PIC 9(04)   VALUE ZEROES.

       01  TR1534-FILE-BEG-TIME                PIC 9(06)   VALUE ZEROES.
       01  FILLER REDEFINES TR1534-FILE-BEG-TIME.
           03  TR1534-FILE-BEG-TIME-HH         PIC 9(02).
           03  TR1534-FILE-BEG-TIME-MM         PIC 9(02).
           03  TR1534-FILE-BEG-TIME-SS         PIC 9(02).

       01  TR1534-FILE-INPUT                   PIC X(22)   VALUE SPACES.
       01  FILLER                    REDEFINES TR1534-FILE-INPUT.
           03  TR1534-FILE-INPUT-DIR           PIC X(02).
           03  TR1534-FILE-INPUT-SLASH         PIC X(01).
           03  TR1534-FILE-INPUT-NAME          PIC X(19).

      *-----------------------------------------------------------------
       78  TR1534-FILE-ORDER---SOURCE                      VALUE "S".
       78  TR1534-FILE-ORDER---TARGET                      VALUE "T".
       77  TR1534-FILE-ORDER                   PIC X(01)   VALUE SPACES.
           88  TR1534-FILE-ORDER-SOURCE                    VALUE "S".
           88  TR1534-FILE-ORDER-TARGET                    VALUE "T".

      *-----------------------------------------------------------------
       78  TR1534-FILE-TYPE---BASESH                       VALUE "B".
       78  TR1534-FILE-TYPE---GLOBAL                       VALUE "G".
       78  TR1534-FILE-TYPE---LOCAL                        VALUE "L".
       77  TR1534-FILE-TYPE                    PIC X(01)   VALUE SPACES.
           88  TR1534-FILE-TYPE-BASESH                     VALUE "B".
           88  TR1534-FILE-TYPE-GLOBAL                     VALUE "G".
           88  TR1534-FILE-TYPE-LOCAL                      VALUE "L".

      *=================================================================
       01  TR1534-FILE-PATH                          PIC X(35).

       01  FILLER                          REDEFINES TR1534-FILE-PATH.
           03  FILLER                                PIC X(01).
           03  TR1534-FILE-PATH-FIL                  PIC X(34).

       01  FILLER                          REDEFINES TR1534-FILE-PATH.
           03  TR1534-FILE-PATH-SLASH-1              PIC X(01).
           03  TR1534-FILE-PATH-DIR-1                PIC X(03).
           03  TR1534-FILE-PATH-SLASH-2              PIC X(01).
           03  TR1534-FILE-PATH-DIR-2                PIC X(04).
           03  TR1534-FILE-PATH-SLASH-3              PIC X(01).
           03  TR1534-FILE-PATH-MACHINE              PIC X(02).
           03  TR1534-FILE-PATH-SLASH-4              PIC X(01).
           03  TR1534-FILE-PATH-GLOBAL-LOCAL         PIC X(22).

       01  FILLER                          REDEFINES TR1534-FILE-PATH.
           03  FILLER                                PIC X(13).
           03  TR1534-FILE-PATH-GLOBAL-PATH.
               05  TR1534-FILE-PATH-GLOBAL-DIR       PIC X(02).
               05  TR1534-FILE-PATH-GLOBAL-SLASH     PIC X(01).
               05  TR1534-FILE-PATH-GLOBAL-FILE      PIC X(19).

       01  FILLER                          REDEFINES TR1534-FILE-PATH.
           03  TR1534-FILLER                         PIC X(13).
           03  TR1534-FILE-PATH-LOCAL-DATA           PIC X(06).
           03  TR1534-FILE-PATH-LOCAL-DATA-SL        PIC X(01).
           03  TR1534-FILE-PATH-LOCAL-PATH.
               05  TR1534-FILE-PATH-LOCAL-DIR        PIC X(02).
               05  TR1534-FILE-PATH-LOCAL-SLASH      PIC X(01).
               05  TR1534-FILE-PATH-LOCAL-FILE       PIC X(12).

      *=================================================================
       01  TR1534-TIME-BEG                     PIC 9(06)   VALUE ZEROES.
       01  TR1534-TIME-BEGX          REDEFINES TR1534-TIME-BEG
                                               PIC X(06).
       01  FILLER                    REDEFINES TR1534-TIME-BEG.
           03  TR1534-TIME-BEG-HH              PIC 9(02).
           03  TR1534-TIME-BEG-MM              PIC 9(02).
           03  TR1534-TIME-BEG-SS              PIC 9(02).

       01  TR1534-TIME-END                     PIC 9(06)   VALUE ZEROES.
       01  TR1534-TIME-ENDX          REDEFINES TR1534-TIME-END
                                               PIC X(06).
       01  FILLER                    REDEFINES TR1534-TIME-END.
           03  TR1534-TIME-END-HH              PIC 9(02).
           03  TR1534-TIME-END-MM              PIC 9(02).
           03  TR1534-TIME-END-SS              PIC 9(02).

       01  TR1534-TIME-ELAPSED                 PIC 9(06)   VALUE ZEROES.
       01  FILLER REDEFINES TR1534-TIME-ELAPSED.
           03  TR1534-TIME-ELAPSED-HH          PIC 9(02).
           03  TR1534-TIME-ELAPSED-MM          PIC 9(02).
           03  TR1534-TIME-ELAPSED-SS          PIC 9(02).

       01  TR1534-TIME-SECONDS                 PIC 9(06)   VALUE ZEROES.

      ******************************************************************
      *
      *    T R 1 5 3 0 - D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  TR1534-LN-FEED                     PIC 9(03)    VALUE ZEROES.

      *=================================================================
       01  TR1534-DETAIL-LINE                 PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  TR1534-DETAIL-LINE-1     REDEFINES TR1534-DETAIL-LINE.
           03  TR1534-D1-BRANCH-PROMPT        PIC X(08).
           03  TR1534-D1-BRANCH               PIC ZZZ9.
           03  FILLER                         PIC X(02).
           03  TR1534-D1-BRANCH-NAME          PIC X(66).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-2            REDEFINES TR1534-DETAIL-LINE.
           03  TR1534-D2-MACHINE-PROMPT       PIC X(09).
           03  TR1534-D2-MACHINE              PIC X(02).
           03  FILLER                         PIC X(03).
           03  TR1534-D2-MACHINE-NAME         PIC X(66).

      *-----------------------------------------------------------------
       01  TR1534-FILE-LINE-1       REDEFINES TR1534-DETAIL-LINE.
           03  TR1534-F1-FILE-PROMPT          PIC X(06).
           03  TR1534-F1-FILE                 PIC X(10).
           03  FILLER                         PIC X(02).
           03  TR1534-F1-FILE-MESSAGE         PIC X(62).
           03  FILLER               REDEFINES TR1534-F1-FILE-MESSAGE.
               05  TR1534-F1-RECORD-READ-PROMPT   PIC X(06).
               05  TR1534-F1-RECORD-READ          PIC Z,ZZZ,ZZZ,ZZ9.
               05  FILLER                         PIC X(02).
               05  TR1534-F1-RECORD-WRITE-PROMPT  PIC X(07).
               05  TR1534-F1-RECORD-WRITE         PIC Z,ZZZ,ZZZ,ZZ9.
               05  FILLER                         PIC X(02).
               05  TR1534-F1-RECORD-SKIP-PROMPT   PIC X(06).
               05  TR1534-F1-RECORD-SKIP          PIC Z,ZZZ,ZZZ,ZZ9.
           03  FILLER                         PIC X(02).
           03  TR1534-F1-TIMES                PIC X(50).
           03  FILLER               REDEFINES TR1534-F1-TIMES.
               05  FILLER                     PIC X(05).
               05  TR1534-F1-BEG-PROMPT       PIC X(05).
               05  TR1534-F1-BEG-TIME         PIC 99/99/99.
               05  FILLER                     PIC X(02).
               05  TR1534-F1-END-PROMPT       PIC X(05).
               05  TR1534-F1-END-TIME         PIC 99/99/99.
               05  FILLER                     PIC X(02).
               05  TR1534-F1-TOT-PROMPT       PIC X(07).
               05  TR1534-F1-TOT-TIME         PIC 99/99/99.

      ******************************************************************
      *
      *    T R 1 5 3 4 - M V 1 5 - F I L E - W O R K E R S
      *
      *=================================================================
      * DUE TO ACU4GL, THE A15 FILE NEEDS TO BE RENAMED (VIA MOVE; MV)
      * SO IT IDENTIFIES THE CORRECT FILE.
      * FOR EXAMPLE, USING THE STFILE, THE A15 FILE OF
      * /US1/DA15/R1/LP/STFILE IS SEEN AS THE A34 STFILE DATABASE TABLE.
      * WHEN TRYING TO OPEN THE A15 FILE IT WILL RESULT IN AN ACU ERROR
      * OF "9D,,2".  FOR THE UPGRADE PROGRAMS TO WORK, NEED TO:
      * 
      *  1. MOVE /US1/DA15/R1/LP/STFILE TO /US1/DA15/R1/LP/OSTFILE
      *  2. OPEN, READ-NEXT, CLOSE ON THE OSTFILE.
      *  3. MOVE /US1/DA15/R1/LP/OSTFILE TO /US1/DA15/R1/LP/STFILE
      * 
      * NEED TO MOVE THE FILE BACK TO ITS ORIGINAL NAME IN CASE THE
      * UPGRADE IS BEING EXECUTED MULTIPLE TIMES (QA TESTING) OR FOR
      * SIDE-BY-SIDE TESTING BETWEEN A15 & A34.
      *
      *=================================================================
      *  BASESH - /US1/DA15/SH/YYFILE
      *  GLOBAL - /US1/DA15/R1/XX/YYFILE
      *  LOCAL  - /US1/DA15/R1/SS####/XX/YYFILE
      *           ....|....1....|....2....|....3
      *
      ******************************************************************
       01  TR1534-MV15-OLD-PATH                  PIC X(35).

       01  FILLER                      REDEFINES TR1534-MV15-OLD-PATH.
           03  FILLER                            PIC X(13).
           03  TR1534-MV15-OLD-BASESH-FILE       PIC X(22).

       01  FILLER                      REDEFINES TR1534-MV15-OLD-PATH.
           03  FILLER                            PIC X(16).
           03  TR1534-MV15-OLD-GLOBAL-FILE       PIC X(19).

       01  FILLER                      REDEFINES TR1534-MV15-OLD-PATH.
           03  FILLER                            PIC X(23).
           03  TR1534-MV15-OLD-LOCAL-FILE        PIC X(12).

      *-----------------------------------------------------------------
       01  TR1534-MV15-NEW-PATH                  PIC X(35).

       01  FILLER                      REDEFINES TR1534-MV15-NEW-PATH.
           03  FILLER                            PIC X(13).
           03  TR1534-MV15-NEW-BASESH-O          PIC X(01).
           03  TR1534-MV15-NEW-BASESH-FILE       PIC X(21).

       01  FILLER                      REDEFINES TR1534-MV15-NEW-PATH.
           03  FILLER                            PIC X(16).
           03  TR1534-MV15-NEW-GLOBAL-O          PIC X(01).
           03  TR1534-MV15-NEW-GLOBAL-FILE       PIC X(18).

       01  FILLER                      REDEFINES TR1534-MV15-NEW-PATH.
           03  FILLER                            PIC X(23).
           03  TR1534-MV15-NEW-LOCAL-O           PIC X(01).
           03  TR1534-MV15-NEW-LOCAL-FILE        PIC X(11).

      ******************************************************************
      *
      *    T R 1 5 3 0 - V P I - W O R K E R S 
      *
      *=================================================================
      *    TR1534 VIDEO PROGRESS INDICATOR (VPI)
      ******************************************************************
       01  TR1534-VPI-BRNO                 PIC 9(04)   VALUE ZEROES.

       01  TR1534-VPI-PRU-LP               PIC X(03)   VALUE "HLD".
           88  TR1534-VPI-QUEUE-PIGGYBACK              VALUE "PRU"
                                                             "DEV".
           88  TR1534-VPI-QUEUE-PRINTER                VALUE "   ".
           88  TR1534-VPI-QUEUE-TERMINAL               VALUE "VDU".
           88  TR1534-VPI-QUEUE-VIEW                   VALUE "HLD".
           88  TR1534-VPI-QUEUE-PRINTER-OR-VIEW        VALUE "   "
                                                             "HLD".

       01  TR1534-VPI-ROUTE-BUF            PIC X(02)   VALUE "00".
           88  TR1534-VPI-ROUTE-MENU-DRIVEN            VALUE "00" "  ".

       01  TR1534-VPI-SLEEP-COUNTER        PIC 9(04)   VALUE ZEROES.
       01  TR1534-VPI-SLEEP-COUNTER-MAX    PIC 9(04)   VALUE 500.
       01  TR1534-VPI-TICKER               PIC X(01)   VALUE "-".
       01  TR1534-VPI-TIME-EDIT            PIC X(08)   VALUE SPACES.

      *=================================================================
      *
      *    T R 1 5 3 0 - V P I - D I S P L A Y  - W O R K E R S
      *
      *=================================================================
       01  TR1534-VPI-DISPLAY-BUF               PIC X(80)  VALUE SPACES.
       01  FILLER                     REDEFINES TR1534-VPI-DISPLAY-BUF.
           03  TR1534-VPI-DISPLAY-LEFT          PIC X(01).
           03  TR1534-VPI-DISPLAY-TICKER        PIC X(01).
           03  TR1534-VPI-DISPLAY-RIGHT         PIC X(01).
           03  FILLER                           PIC X(01).
           03  TR1534-VPI-DISPLAY-TEXT          PIC X(76).
           03  FILLER                 REDEFINES TR1534-VPI-DISPLAY-TEXT.
               05  TR1534-VPI-DISPLAY-BRNO      PIC 9(04).
               05  FILLER                       PIC X(02).
               05  TR1534-VPI-DISPLAY-FILE      PIC X(60).
               05  FILLER                       PIC X(02).
               05  TR1534-VPI-DISPLAY-TIME      PIC X(08).

       01  TR1534-VPI-DISPLAY-LIST.
           03  FILLER PIC X(14) VALUE "LEGEND ACCEPT.".

      *=================================================================
      *
      *    T R 1 5 3 0 - V P I - F I L E - W O R K E R S
      *
      *=================================================================
       78  TR1534-VPI-FILE-TYPE---GLOBAL                 VALUE "G".
       78  TR1534-VPI-FILE-TYPE---LOCAL                  VALUE "L".
       77  TR1534-VPI-FILE-TYPE            PIC X(01)     VALUE SPACES.
           88  TR1534-VPI-FILE-TYPE-GLOBAL               VALUE "G".
           88  TR1534-VPI-FILE-TYPE-LOCAL                VALUE "L".

      *-----------------------------------------------------------------
       01  TR1534-VPI-FILE-PREFIX         PIC X(06)   VALUE SPACES.
           88  TR1534-VPI-FILE-CH                     VALUE "CH".
           88  TR1534-VPI-FILE-TR                     VALUE "TR".
      *- - - - - - - - - - -{ MISC. }- - - - - - - - - - -{ MISC. }- - -
           88  TR1534-VPI-FILE-WK1                    VALUE "WK1".
           88  TR1534-VPI-FILE-WK2                    VALUE "WK2".
           88  TR1534-VPI-FILE-REPORT                 VALUE "REPORT".

      *-----------------------------------------------------------------
       01  TR1534-VPI-FILE-MMYY           PIC 9(04)   VALUE ZEROES.

      ******************************************************************
      *
      *    S C R E E N - W O R K E R S
      *
      ******************************************************************
       01  DEFAULT-GLOBAL-OPTIONS      PIC X(01)     VALUE SPACES.
       01  DEFAULT-LOCAL-OPTIONS       PIC X(01)     VALUE SPACES.

       01  COPIES-LIST                 PIC X(07)     VALUE "COPIES.".
       01  EOBUF-COPIES                PIC 9(01).

       01  CUSTOMER-LINE-LIST          PIC X(11)    VALUE "START-TIME.".
       01  CUSTOMER-LINE-BUF.
           03  CUSTOMER-RIGHT-SIDE     PIC X(02)     VALUE SPACES.
           03  CUSTOMER-ID             PIC X(08)     VALUE SPACES.
           03  CUSTOMER-LEFT-SIDE      PIC X(02)     VALUE SPACES.

       01  FIL-LIST                    PIC X(11)    VALUE "DATA-SET:B.".
       01  FIL-BUF.
           03  FIL-PROMPT              PIC X(13)     VALUE SPACES.
           03  FIL-INFO                PIC X(35)     VALUE SPACES.

       01  MACHINE-LIST                PIC X(12)   VALUE "MMACHINE:BF.".
       01  MACHINE-BUF.
           03  MACHINE-PROMPT          PIC X(18)     VALUE SPACES.
           03  MACHINE-NAME            PIC X(02)     VALUE SPACES.

       01  PRU-BUF                     PIC 9(02).
       01  PRU-LIST                    PIC X(04)     VALUE "PRU.".

      *-----------------------------------------------------------------
       01  SAVE-REPORT-COMMAND.
           03  SAVE-REPORT-CMD         PIC X(03)   VALUE "MV ".
           03  SAVE-REPORT-SOURCE      PIC X(35)   VALUE SPACES.
           03  FILLER                  PIC X(01)   VALUE SPACES.
           03  SAVE-REPORT-TARGET      PIC X(35)   VALUE SPACES.
           03  FILLER                  REDEFINES SAVE-REPORT-TARGET.
               05  FILLER              PIC X(09).
               05  SAVE-REPORT-TARGET-NEW
                                       PIC X(07).
               05  SAVE-REPORT-TARGET-MKTEMP
                                       PIC X(19).

      *-----------------------------------------------------------------
       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".
       01  TARGET-PATH-LIST            PIC X(07)     VALUE "T-PATH.".

       01  STARTING-TIME-LIST          PIC X(11)    VALUE "START-TIME.".
       01  STARTING-TIME-BUF.
           03  STARTING-TIME-RIGHT     PIC X(02)     VALUE SPACES.
           03  STARTING-TIME           PIC X(08)     VALUE SPACES.
           03  STARTING-TIME-LEFT      PIC X(02)     VALUE SPACES.

      *=================================================================
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------

           03  TR1534-MACHINE          PIC X(02).
           03  TR1534-SOURCE-PATH      PIC X(35).
           03  TR1534-TARGET-PATH      PIC X(35).

           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).

           03  GROUP-OPTION-WORKERS.
               05  GROUP-FLAG          PIC X(01).
               05  ENT-GROUP           PIC X(04).

           03  BEG-DATE-YYYYMM         PIC 9(06).
           03  END-DATE-YYYYMM         PIC 9(06).

           03  FILE-OPTION-CH             PIC X(01).
           03  FILE-OPTION-TR           PIC X(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

       01  TR1534-PRU-LP               PIC X(03).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
      *------------------------------------------{ NEED TO BE TOGETHER }
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUP/TR1534_DEF.CPY".
       COPY "LIBUP/TR1534_WKS.CPY".
     
      ******************************************************************
      *
      *    L I N K A G E - S C R E E N - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
     
      *=================================================================
       SCREEN SECTION.
       COPY "LIBUP/TR1534_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 OBR-FILE SORT-FILE MSEXCEL-FILE REPORT-FILE
                 OCH-FILE OTR-FILE WK-FILE
      * ASCII OUTPUT FILES
                 ATRB-FILE ATRC-FILE ATRD-FILE ATRI-FILE ATRJ-FILE
                 ATRK-FILE ATRL-FILE ATRP-FILE ATRS-FILE ATRW-FILE
                 ATRX-FILE ACH-FILE
                 ATRB-DONE-FILE ATRC-DONE-FILE ATRD-DONE-FILE
                 ATRI-DONE-FILE ATRJ-DONE-FILE ATRK-DONE-FILE
                 ATRL-DONE-FILE ATRP-DONE-FILE ATRS-DONE-FILE
                 ATRW-DONE-FILE ATRX-DONE-FILE ACH-DONE-FILE.

       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TRX1-FILE.
           PERFORM CLOSE-TRW1-FILE.
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-TRI1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE OBR-FILE SORT-FILE MSEXCEL-FILE WK-FILE
                 OCH-FILE OTR-FILE
      * ASCII OUTPUT FILES
                 ATRB-FILE ATRC-FILE ATRD-FILE ATRI-FILE ATRJ-FILE
                 ATRK-FILE ATRL-FILE ATRP-FILE ATRS-FILE ATRW-FILE
                 ATRX-FILE REPORT-FILE ACH-FILE
                 ATRB-DONE-FILE ATRC-DONE-FILE ATRD-DONE-FILE
                 ATRI-DONE-FILE ATRJ-DONE-FILE ATRK-DONE-FILE
                 ATRL-DONE-FILE ATRP-DONE-FILE ATRS-DONE-FILE
                 ATRW-DONE-FILE ATRX-DONE-FILE ACH-DONE-FILE.

       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
          
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM MISC-TRACE-INITIALIZATION.           *> ENABLE TRACE

      * CREATE WORK FILE FOR TRANSFILE FILE PAYMENTS/POOLID 
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              IF ( EXT-ROUTE-BUF = "00" )
                 MOVE "CANNOT MAKE WORK FILE" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "ERROR: CANNOT MAKE WORK FILE"
                   TO EXT-TCLP-TRACE-MESS
                 MOVE FORM-PATHNAME       TO EXT-TCLP-TRACE-FORM-PATH
                 PERFORM TRACE-SEND-MESS
              END-IF
              GO TO MAIN-PROGRAM-EXIT.

           MOVE MKTEMP-BUF                 TO WK-PATH.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

      * LOAD A TABLE INDEXED BY BR-NO JUST TO KNOW THAT BR-MACHINE.
      * USING THE ORIGINAL A15 BRANCH FILE IN CASE YOU ALREADY CHANGED
      * THE NEW FILE TO ALL BE R1 BRANCHES!
      * BRANCH 9990 WILL BE R1
           PERFORM LOAD-BRANCH-TABLE.
           IF ( ERRCD NOT = SPACES )
              IF ( EXT-ROUTE-BUF = "00" )
                 MOVE "NEED ORIGINAL BRANCH FILE FOR MACHINE" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "ERROR: NEED ORIGINAL BRANCH FILE FOR MACHINE"
                   TO EXT-TCLP-TRACE-MESS
                 MOVE FORM-PATHNAME       TO EXT-TCLP-TRACE-FORM-PATH
                 PERFORM TRACE-SEND-MESS
              END-IF
              GO TO MAIN-PROGRAM-EXIT.

      * CREATE ASCII AND DONE DIRECTORIES IF THEY DONT EXIST
            MOVE TR1534-TARGET-PATH  TO ASCII-DIR-BEG.
            MOVE TR1534-MACHINE      TO ASCII-DIR-MACHINE.
            CALL "C$TOLOWER" USING ASCII-DIR-NAME, VALUE 06.

      * /USR/DATA/R1/ASCII
           MOVE ASCII-DIRECTORY-PATH TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING ASCII-DIRECTORY-PATH GIVING STAT-99
              IF STAT NOT = "00"
                 IF ( EXT-ROUTE-BUF = "00" )
                    MOVE "CAN'T CREATE ASCII DIRECTORY" TO MESS
                    PERFORM SEND-MESS
                 ELSE
                    MOVE "ERROR: CAN'T CREATE ASCII DIRECTORY"
                      TO EXT-TCLP-TRACE-MESS
                    MOVE FORM-PATHNAME       TO EXT-TCLP-TRACE-FORM-PATH
                    PERFORM TRACE-SEND-MESS
                 END-IF
                 GO TO MAIN-PROGRAM-EXIT.

      * /USR/DATA/R1/ASCII/DONE
           MOVE "/DONE" TO ASCII-DIR-DONE.
           CALL "C$TOLOWER" USING ASCII-DIR-DONE, VALUE 05.
           MOVE ASCII-DIRECTORY-PATH TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING ASCII-DIRECTORY-PATH GIVING STAT-99
              IF STAT NOT = "00"
                 IF ( EXT-ROUTE-BUF = "00" )
                    MOVE "CAN'T CREATE ASCII/DONE DIRECTORY" TO MESS
                    PERFORM SEND-MESS
                 ELSE
                    MOVE "ERROR: CAN'T CREATE ASCII/DONE DIRECTORY"
                      TO EXT-TCLP-TRACE-MESS
                    MOVE FORM-PATHNAME       TO EXT-TCLP-TRACE-FORM-PATH
                    PERFORM TRACE-SEND-MESS
                 END-IF
                 GO TO MAIN-PROGRAM-EXIT.
 
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO HOLD-ASCII-PATH-GROUP
           ELSE
              IF (BEG-BRANCH = 0 AND END-BRANCH = 9999)
                 MOVE "ALL" TO HOLD-ASCII-PATH-GROUP
              ELSE
                 MOVE BEG-BRANCH TO HOLD-ASCII-PATH-BRNO.

      * LOCAL FILE UPGRADES
      * NEED TO OPEN OUTPUT BEFORE IT ALL GETS STARTED. OPEN I-O WILL
      * NOT CREATE THEM!
           PERFORM OPEN-OUTPUT-ALL-ASCII-FILES.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       MAIN-PROGRAM-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = ZEROES )
              GO TO MAIN-PROGRAM-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MAIN-PROGRAM-PROCESS-BRNO.

       MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD ) OR ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-PROCESS-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * JKC 2024-1216 SKIPPING BRANCH 9991 DUE TO A15 BRFILE SAYING
      *     IT IS A 'R1' WHEN S35 HAS IT AS 'R2'; THERE IS NO DATA
      *     FOR 'R2' BUT THERE IS FOR 'R1' WHICH IS THROWING OFF
      *     COMPARISONS AND COUNTS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BR-NO = 9991 )
              GO TO MAIN-PROGRAM-NEXT-GROUP.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           PERFORM TR1534-CREATE-BRANCH-HEAD.

           PERFORM PROCESS-FILE-TR.
           PERFORM PROCESS-FILE-CH.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

      *=================================================================
       MAIN-PROGRAM-END.

      * CREATE DONE TAGS EVEN IF NO RECORDS FOUND, NEED TO KNOW ITS DONE
           IF NOT ( TR1534-EXIT-PRIOR-TO-EXECUTION )
              PERFORM CREATE-DONE-TAGS.
           PERFORM PRINT-REPORT-MODULE.
           PERFORM EXTRACT-ROUTINE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      *****************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.

           MOVE "X"                        TO TR1534-RECORDS-FOUND.
           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "TR1534"                   TO VDU-FORM-NAME.
           MOVE TR1534-QUEUE-POS           TO Q-POS.
           MOVE TR1534-COPIES-POS          TO C-POS.
           MOVE TR1534-PRU-POS             TO P-POS.
           MOVE SPACES                     TO Q-BUF  C-BUF  P-BUF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/EOINIT.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH
                                              END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE GROUP-FLAG              TO SV-GROUP-FLAG
              MOVE ENT-GROUP               TO SV-ENT-GROUP
              MOVE ZEROES                  TO BEG-BRANCH
              MOVE 9999                    TO END-BRANCH
              MOVE "Y"                     TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP         TO ENT-GROUP
              MOVE EXT-EOBUF-GROUP-PRPATH  TO PRU-FILE.

           PERFORM OPEN-PR-FILE.
           MOVE PRU-LP                     TO TR1534-PRU-LP.
           MOVE "N"                        TO TR1534-RECORDS-FOUND.
           PERFORM SEND-STARTING-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ SETUP-HEADER }
           MOVE EXT-CONAME-CONAME          TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE         TO H1-DATE.
           MOVE EXT-CONAME-USER-ID         TO H2-USERID.

      *- - - - - - - - - - - - - - - - - - - - - -{ MAKING TEMP FILES  }
           MOVE MKTEMP-DEFAULT             TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF                 TO TR1534-REPORT-PATH.
           PERFORM OPEN-REPORT-FILE-OUTPUT.
           PERFORM CLOSE-REPORT-FILE.
           MOVE ZEROES TO TR1534-REPORT-KEY.


           PERFORM LOAD-SORT-FILE.
           PERFORM OPEN-SORT-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-REPORT-FILE.

       EXIT-INITIALIZATION.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARAMETERS-BEGIN.

           PERFORM SEND-TITLE.
           PERFORM SEND-FIL.
           PERFORM SEND-MACHINE.
           PERFORM SEND-CUSTOMER-LINE.

           MOVE TR1534-SOURCE-PATH         TO WR-BUF.
           MOVE SOURCE-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE TR1534-TARGET-PATH         TO WR-BUF.
           MOVE TARGET-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP-KEY )
              GO TO ENTER-OPTION-CH.

      *=================================================================
       ENTER-SOURCE-PATH.

           MOVE LEGEND-SOURCE-PATH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A090S-PATH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-SOURCE-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = SPACES )
              GO TO ENTER-SOURCE-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SOURCE FILE ACCESS CHECK:
      *    - TO PREVENT BOMBING OUT IF BRANCH'S FILES NOT ON THIS SYSTEM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO DIR-ACCESS-BUF.

           STRING VDUBUF, "/", TR1534-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.

           PERFORM DIR-ACCESS-CALL.

           IF ( STAT-BAD )
              PERFORM INVALID-FILE-PATH-MESSAGE
              GO TO ENTER-SOURCE-PATH.

           MOVE VDUBUF                     TO TR1534-SOURCE-PATH.

      *-----------------------------------------------------------------
       ENTER-TARGET-PATH.

           MOVE LEGEND-TARGET-PATH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A090T-PATH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-SOURCE-PATH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = SPACES )
              GO TO ENTER-TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TARGET FILE ACCESS CHECK:
      *    - TO PREVENT BOMBING OUT IF BRANCH'S FILES NOT ON THIS SYSTEM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO DIR-ACCESS-BUF.

           STRING VDUBUF, "/", TR1534-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.

           PERFORM DIR-ACCESS-CALL.

           IF ( STAT-BAD )
              PERFORM INVALID-FILE-PATH-MESSAGE
              GO TO ENTER-TARGET-PATH.

           MOVE VDUBUF                     TO TR1534-TARGET-PATH.

           IF ( TR1534-SOURCE-PATH = TR1534-TARGET-PATH )
              PERFORM INVALID-SAME-PATHS-MESSAGE
              GO TO ENTER-SOURCE-PATH.

      *=================================================================
      *    ENTERING BRANCH NUMBER RANGE.
      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.
       
           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.
       
           IF ( UP-KEY ) GO TO ENTER-TARGET-PATH.
           IF ( F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF ( F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF ( F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
       
           IF ( F6-KEY )
              PERFORM SCAN-GR-FILE
              IF ( IO-BAD )
                 GO TO ENTER-BEG-BRANCH.
       
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-BRANCH.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
       
           IF ( WGR-WORD-ZERO )
              GO TO BRANCH-IS-NUMERIC.
       
           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
       
           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.
       
           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.
       
      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
       
           MOVE "N"                        TO GROUP-FLAG.
           MOVE WGR-SRESULT                TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH."       TO VDU.
           MOVE BEG-BRANCH                 TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.
       
           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.
       
           IF ( UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF ( F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF ( F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
       
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-BRANCH.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.
       
           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "N"                  TO GROUP-FLAG.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO VDUNUM0 BEG-BRANCH.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO VDUNUM0 END-BRANCH.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
       
           PERFORM BR-RANGE-SECURITY.
       
           IF ( BR-SECURITY-OKAY = "N" )
              GO TO ENTER-BEG-BRANCH.

      *=================================================================
       ENTER-BEG-DATE.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO ENTER-OPTION-TR.

           MOVE LEGEND-BEG-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-BEG-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO DATE-MMYY.

           IF ( DATE-MMYY-MM < 01 ) OR
              ( DATE-MMYY-MM > 12 )
              MOVE "ERROR: ENTRY NOT VALID (MMYY)" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BEG-DATE-YYYYMM   WSS-DATE-YYYYSMM.

           MOVE "S  A040BEG-YYYYMM." TO VDU.
           MOVE WSS-DATE-YYYYSMM     TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-END-DATE.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO ENTER-END-BRANCH.

           MOVE LEGEND-END-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-END-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO DATE-MMYY.

           IF ( DATE-MMYY-MM < 01 ) OR
              ( DATE-MMYY-MM > 12 )
              MOVE "ERROR: ENTRY NOT VALID (MMYY)" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO END-DATE-YYYYMM   WSS-DATE-YYYYSMM.

           MOVE "S  A040END-YYYYMM." TO VDU.
           MOVE WSS-DATE-YYYYSMM     TO VDUBUF.
           PERFORM SCREENIO.

           IF ( BEG-DATE-YYYYMM > END-DATE-YYYYMM )
              MOVE "ERROR: DATE RANGE INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

      *========================================{ SOURCE & TARGET FILES }
       ENTER-OPTION-TR.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-TR." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-TR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-TR.

           IF NOT ( FILE-OPTION-TR = "Y" OR "N" OR "C" OR "A" )
              PERFORM INVALID-YNC-MESSAGE
              GO TO ENTER-OPTION-TR.

      *-----------------------------------------------------------------
       ENTER-OPTION-CH.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-CH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-TR.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-CH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-CH.

           IF NOT ( FILE-OPTION-CH = "Y" OR "N" OR "C" OR "A" )
              PERFORM INVALID-YNC-MESSAGE
              GO TO ENTER-OPTION-CH.

      *- - - - - - - - - - - - - - - - - - - - -{ SKIP TO 'ACCEPT Y/N' }
           GO TO ENTER-ACCEPT-YN.

      *========================================{ SOURCE & TARGET FILES }
       ENTER-ALL-OPTIONS.

           IF ( F2-KEY )
              MOVE "C" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "C" TO DEFAULT-LOCAL-OPTIONS
           ELSE
           IF ( F3-KEY )
              MOVE "Y" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "Y" TO DEFAULT-LOCAL-OPTIONS
           ELSE
           IF ( F4-KEY )
              MOVE "N" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "N" TO DEFAULT-LOCAL-OPTIONS
           ELSE
           IF ( F5-KEY )
              MOVE "Y" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "N" TO DEFAULT-LOCAL-OPTIONS
           ELSE
              MOVE "N" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "Y" TO DEFAULT-LOCAL-OPTIONS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010OPTION-TR."  TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS  TO VDUBUF
                                          FILE-OPTION-TR.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-CH."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS  TO VDUBUF
                                          FILE-OPTION-CH.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

           PERFORM SEND-MACHINE.

           MOVE "S  A090S-PATH."          TO VDU.
           MOVE TR1534-SOURCE-PATH        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A090T-PATH."          TO VDU.
           MOVE TR1534-TARGET-PATH        TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."   TO VDU
              MOVE ENT-GROUP              TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."   TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."   TO VDU
              MOVE BEG-BRANCH             TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."   TO VDU
              MOVE END-BRANCH             TO VDUNUM0
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010OPTION-TR."    TO VDU.
           MOVE FILE-OPTION-TR  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-CH."      TO VDU.
           MOVE FILE-OPTION-CH    TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

           MOVE EXT-EOBUF-DATE1            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM                TO BEG-DATE-YYYYMM.

           MOVE EXT-EOBUF-DATE2            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM                TO END-DATE-YYYYMM.

      *=================================================================
       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - B R A N C H - T A B L E
      *
      *=================================================================
      * LOAD A BRANCH TABLE ONE TIME INDEXED BY BR-NO FOR BR-MACHINE
      ******************************************************************
        LOAD-BRANCH-TABLE SECTION.

           MOVE SPACES TO ERRCD.

           MOVE "BR"                       TO TR1534-CURRENT-FILE.
           MOVE "GB/BRFILE"                TO TR1534-FILE-INPUT.
           MOVE TR1534-FILE-TYPE---GLOBAL  TO TR1534-FILE-TYPE.

           MOVE TR1534-FILE-ORDER---SOURCE TO TR1534-FILE-ORDER.
           PERFORM TR1534-LOAD-FILE-PATH.
      *KC  MOVE TR1534-FILE-PATH           TO TR1534-MV15-OLD-PATH.
      *KC  PERFORM TR1534-MV15-OLD-TO-NEW.
      *KC  MOVE TR1534-MV15-NEW-PATH       TO OBR-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * JKC 2024-0325 CHANGED TO NOT MOVE THE A15 BRFILE TO BE OBRFILE;
      *     THIS WAS OLD ACU4GL LOGIC WHERE WE NEED TO MAKE IT OBRFILE;
      *     THIS OLD LOGIC CAUSED A LOCKING ISSUE WHEN RUNNING
      *     MULTIPLE SESSIONS VIA BATCHES (HAD 8 SESSIONS).
      *     NOW GOING TO USE DIRECTLY THE A15 BRFILE AS IS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TR1534-FILE-PATH           TO OBR-PATH.

      *KC  IF ( STAT-BAD )     *> MOVE OF OLD FILE TO NEW FILE WAS BAD
      *KC     MOVE "X" TO ERRCD
      *KC     GO TO LOAD-BRANCH-TABLE-EXIT.

           PERFORM OPEN-OBR-FILE.

           MOVE 1 TO OBR-KEY.
           PERFORM START-OBR-FILE.

        LOAD-NEXT-BRANCH.
           PERFORM READ-OBR-FILE-NEXT.
           IF IO-BAD
              MOVE SPACES   TO ERRCD
      *KC     MOVE OBR-PATH TO TR1534-MV15-NEW-PATH
      *KC     PERFORM TR1534-MV15-NEW-TO-OLD
              GO TO LOAD-BRANCH-TABLE-EXIT.

           MOVE OBR-MACHINE TO BRANCH-MACHINE(OBR-NO).

           GO TO LOAD-NEXT-BRANCH.

       LOAD-BRANCH-TABLE-EXIT.
           EXIT.

      *****************************************************************
      *
      *    S C A N - G R - F I L E
      *
      *****************************************************************
       SCAN-GR-FILE SECTION.
       
           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF GRSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
              GO TO EXIT-SCAN-GR-FILE.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF GR1-KEY.
           PERFORM SCREENIO.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "00"       TO VDUSTATUS.
           MOVE ZEROES     TO IO-FG.
           MOVE GRSCAN-KEY TO VDUBUF.
       
      *=================================================================
       EXIT-SCAN-GR-FILE.
           EXIT.


     ******************************************************************
       OPEN-OUTPUT-ALL-ASCII-FILES SECTION.
      ******************************************************************

      * LOCAL FILES
           MOVE TR1534-FILE-TYPE---LOCAL   TO TR1534-FILE-TYPE.
           MOVE TR1534-FILE-ORDER---TARGET TO TR1534-FILE-ORDER.

           IF (FILE-OPTION-CH = "A")
              MOVE "LP/CHFILE" TO TR1534-FILE-INPUT
              PERFORM TR1534-LOAD-FILE-PATH
              MOVE TR1534-FILE-PATH TO ACH-PATH-BEG
                                       ACH-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ACH-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ACH-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ACH-PATH-GROUP 
                                            ACH-DONE-PATH-GROUP
              OPEN OUTPUT ACH-FILE
              CLOSE ACH-FILE.
      * ALL TRFILES
           IF (FILE-OPTION-TR = "A")
              MOVE "LP/TRBFILE" TO TR1534-FILE-INPUT
              PERFORM TR1534-LOAD-FILE-PATH
              MOVE TR1534-FILE-PATH TO ATRB-PATH-BEG
                                       ATRB-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRB-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRB-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRB-PATH-GROUP 
                                            ATRB-DONE-PATH-GROUP
              OPEN OUTPUT ATRB-FILE
              CLOSE ATRB-FILE.
           IF (FILE-OPTION-TR = "A")
              MOVE "LP/TRCFILE" TO TR1534-FILE-INPUT
              PERFORM TR1534-LOAD-FILE-PATH
              MOVE TR1534-FILE-PATH TO ATRC-PATH-BEG
                                       ATRC-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRC-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRC-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRC-PATH-GROUP 
                                            ATRC-DONE-PATH-GROUP
              OPEN OUTPUT ATRC-FILE
              CLOSE ATRC-FILE.
           IF (FILE-OPTION-TR = "A")
              MOVE "LP/TRDFILE" TO TR1534-FILE-INPUT
              PERFORM TR1534-LOAD-FILE-PATH
              MOVE TR1534-FILE-PATH TO ATRD-PATH-BEG
                                       ATRD-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRD-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRD-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRD-PATH-GROUP 
                                            ATRD-DONE-PATH-GROUP
              OPEN OUTPUT ATRD-FILE
              CLOSE ATRD-FILE.
           IF (FILE-OPTION-TR = "A")
              MOVE "LP/TRIFILE" TO TR1534-FILE-INPUT
              PERFORM TR1534-LOAD-FILE-PATH
              MOVE TR1534-FILE-PATH TO ATRI-PATH-BEG
                                       ATRI-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRI-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRI-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRI-PATH-GROUP 
                                            ATRI-DONE-PATH-GROUP
              OPEN OUTPUT ATRI-FILE
              CLOSE ATRI-FILE.
           IF (FILE-OPTION-TR = "A")
              MOVE "LP/TRJFILE" TO TR1534-FILE-INPUT
              PERFORM TR1534-LOAD-FILE-PATH
              MOVE TR1534-FILE-PATH TO ATRJ-PATH-BEG
                                       ATRJ-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRJ-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRJ-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRJ-PATH-GROUP 
                                            ATRJ-DONE-PATH-GROUP
              OPEN OUTPUT ATRJ-FILE
              CLOSE ATRJ-FILE.
           IF (FILE-OPTION-TR = "A")
              MOVE "LP/TRKFILE" TO TR1534-FILE-INPUT
              PERFORM TR1534-LOAD-FILE-PATH
              MOVE TR1534-FILE-PATH TO ATRK-PATH-BEG
                                       ATRK-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRK-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRK-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRK-PATH-GROUP 
                                            ATRK-DONE-PATH-GROUP
              OPEN OUTPUT ATRK-FILE
              CLOSE ATRK-FILE.
           IF (FILE-OPTION-TR = "A")
              MOVE "LP/TRLFILE" TO TR1534-FILE-INPUT
              PERFORM TR1534-LOAD-FILE-PATH
              MOVE TR1534-FILE-PATH TO ATRL-PATH-BEG
                                       ATRL-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRL-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRL-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRL-PATH-GROUP 
                                            ATRL-DONE-PATH-GROUP
              OPEN OUTPUT ATRL-FILE
              CLOSE ATRL-FILE.
           IF (FILE-OPTION-TR = "A")
              MOVE "LP/TRPFILE" TO TR1534-FILE-INPUT
              PERFORM TR1534-LOAD-FILE-PATH
              MOVE TR1534-FILE-PATH TO ATRP-PATH-BEG
                                       ATRP-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRP-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRP-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRP-PATH-GROUP 
                                            ATRP-DONE-PATH-GROUP
              OPEN OUTPUT ATRP-FILE
              CLOSE ATRP-FILE.
           IF (FILE-OPTION-TR = "A")
              MOVE "LP/TRSFILE" TO TR1534-FILE-INPUT
              PERFORM TR1534-LOAD-FILE-PATH
              MOVE TR1534-FILE-PATH TO ATRS-PATH-BEG
                                       ATRS-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRS-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRS-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRS-PATH-GROUP 
                                            ATRS-DONE-PATH-GROUP
              OPEN OUTPUT ATRS-FILE
              CLOSE ATRS-FILE.
           IF (FILE-OPTION-TR = "A")
              MOVE "LP/TRWFILE" TO TR1534-FILE-INPUT
              PERFORM TR1534-LOAD-FILE-PATH
              MOVE TR1534-FILE-PATH TO ATRW-PATH-BEG
                                       ATRW-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRW-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRW-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRW-PATH-GROUP 
                                            ATRW-DONE-PATH-GROUP
              OPEN OUTPUT ATRW-FILE
              CLOSE ATRW-FILE.
           IF (FILE-OPTION-TR = "A")
              MOVE "LP/TRXFILE" TO TR1534-FILE-INPUT
              PERFORM TR1534-LOAD-FILE-PATH
              MOVE TR1534-FILE-PATH TO ATRX-PATH-BEG
                                       ATRX-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRX-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRX-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRX-PATH-GROUP 
                                            ATRX-DONE-PATH-GROUP
              OPEN OUTPUT ATRX-FILE
              CLOSE ATRX-FILE.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - C H
      *
      *=================================================================
      *    IN  : BR-REC
      *          TR1534-VPI WORKERS
      *          TR1534-FILE-OPTION-WORKERS
      *
      *    OUT : TR1534-VPI WORKERS
      *          UPGRADE-STAT   [BAD/GOOD]
      *
      *    DESC: PROCESS SOURCE TO TARGET OF GIVEN FILE.
      *
      ******************************************************************
       PROCESS-FILE-CH SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-CH = "N" )
              GO TO PROCESS-FILE-CH-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    START CHFILE DATE LOOP.
      *    CHFILE-YYYYMM NEEDS TO START FROM THE BEGINNING DATE MINUS
      *    ONE MONTH SO TO ALLOW THE ADD LOGIC TO WORK PROPERLY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BEG-DATE-YYYYMM            TO CHFILE-YYYYMM.

           IF ( CHFILE-YYYYMM-MM = 01 )
              SUBTRACT 1                 FROM CHFILE-YYYYMM-YYYY
              MOVE 12                      TO CHFILE-YYYYMM-MM
           ELSE
              SUBTRACT 1                 FROM CHFILE-YYYYMM-MM.

       PROCESS-FILE-CH-DATE.

           ADD 01                          TO CHFILE-YYYYMM-MM.

           IF ( CHFILE-YYYYMM-MM > 12 )
              ADD  01                      TO CHFILE-YYYYMM-YYYY
              MOVE 01                      TO CHFILE-YYYYMM-MM.

           IF ( CHFILE-YYYYMM > END-DATE-YYYYMM )
              GO TO PROCESS-FILE-CH-EXIT.

      *- - - - - - - - - - - - - - - -{ SET CHFILE DATE - CHFILE-MMYY  }
           MOVE CHFILE-YYYYMM              TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY                  TO CHFILE-MMYY.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO CH-PATH-OVERRIDE.

           MOVE "CH"                       TO TR1534-CURRENT-FILE.
           MOVE CHFILE-MMYY                TO TR1534-CURRENT-MMYY.

           MOVE TR1534-FILE-TYPE---LOCAL   TO TR1534-FILE-TYPE.
           MOVE ZEROES                     TO TR1534-RECORD-COUNTERS.

           PERFORM TR1534-LOAD-FILE-BEG-TIME.
           PERFORM TR1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE TR1534-FILE-ORDER---SOURCE TO TR1534-FILE-ORDER.

           MOVE SPACES                     TO TR1534-FILE-INPUT.
           STRING "ED/CH", CHFILE-MMYY
                  DELIMITED BY SIZE      INTO TR1534-FILE-INPUT.

           PERFORM TR1534-LOAD-FILE-PATH.
           MOVE TR1534-FILE-PATH           TO ACCESS-BUF       OCH-PATH.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              GO TO PROCESS-FILE-CH-DATE.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE TR1534-FILE-ORDER---TARGET TO TR1534-FILE-ORDER.

           MOVE "LP/CHFILE"                TO TR1534-FILE-INPUT.
           PERFORM TR1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-CH = "Y")
              MOVE TR1534-FILE-PATH           TO CH-PATH
              PERFORM OPEN-CH-FILE
           ELSE
              MOVE TR1534-FILE-PATH TO ACH-PATH-BEG
                                       ACH-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ACH-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ACH-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ACH-PATH-GROUP 
                                            ACH-DONE-PATH-GROUP
              PERFORM OPEN-ACH-FILE.

           PERFORM MISC-TRACE-FILE-START.

      *- - - - - - - - - - - - - - - - - - -{ CHECK SOURCE FILE STATUS }
           IF ( FILE-OPTION-CH = "C" )
              PERFORM OPEN-OCH-FILE
              PERFORM CLOSE-OCH-FILE
              GO TO PROCESS-FILE-CH-EXIT.

      * IT IS A SEQUENTIAL FILE IN A15, NO START

           PERFORM OPEN-OCH-FILE.

      *=================================================================
       PROCESS-FILE-CH-NEXT.

           PERFORM READ-OCH-FILE.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-CH-END.

           ADD  01  TO TR1534-RECORD-COUNTER-READ.
           MOVE "Y" TO TR1534-RECORDS-FOUND.
           PERFORM TR1534-VPI-DISPLAY.

           PERFORM UPGRADE-FILE-CH.

      *---------------------------------------------{ UPDATE/WRITE CH  }
       PROCESS-FILE-CH-WRITE.

           IF (FILE-OPTION-CH = "Y")
              PERFORM WRITE-CH-FILE
              IF ( IO-BAD )
                 IF ( CH-SEQ < 999 )
                    ADD 1 TO CH-SEQ
                    GO TO PROCESS-FILE-CH-WRITE
                 ELSE
                    MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
                    MOVE "E"            TO TR1534-UPDATE-TYPE
                    PERFORM TR1534-CREATE-FILE-UPDATE
                    ADD 1 TO TR1534-RECORD-COUNTER-SKIP
                    GO TO IO-ERROR.

      * VISION KEY STILL GETS FILLED IN, SO TEST THAT FOR DUPLICATE
      * FIRST TIME IT WILL BE SPACES
      * ALWAYS START OUT HOLD-CH-SEQ AS 1
           IF (FILE-OPTION-CH = "A")
              IF CH-KEY = HOLD-CH-KEY
                 ADD 1 TO HOLD-CH-SEQ
                 MOVE HOLD-CH-SEQ TO ACH-SEQ
              ELSE
                 MOVE 1 TO HOLD-CH-SEQ
              END-IF
              MOVE CH-KEY TO HOLD-CH-KEY
              ADD 1        TO ACH-KEY
              ACCEPT ACH-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ACH-CR
              MOVE X"0A"              TO ACH-LF
              PERFORM WRITE-ACH-FILE
              IF ( IO-BAD )
                 MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
                 MOVE "E"            TO TR1534-UPDATE-TYPE
                 PERFORM TR1534-CREATE-FILE-UPDATE
                 ADD 1 TO TR1534-RECORD-COUNTER-SKIP
                 GO TO IO-ERROR.

           ADD 1 TO TR1534-RECORD-COUNTER-WRITE.

           GO TO PROCESS-FILE-CH-NEXT.

       PROCESS-FILE-CH-END.

           PERFORM CLOSE-OCH-FILE.
           IF (FILE-OPTION-CH = "Y")
              PERFORM CLOSE-CH-FILE
           ELSE
              PERFORM CLOSE-ACH-FILE.

           PERFORM TR1534-CREATE-FILE-TOTALS.
           MOVE "CHFILE"                   TO HOLD-SORT-FILENAME.
           MOVE OCH-PATH                   TO HOLD-SORT-FILEPATH.
           MOVE CH-PATH-OWNST              TO HOLD-SORT-STATE.
           MOVE CH-PATH-OWNBR              TO HOLD-SORT-BRNO.
           PERFORM UPDATE-SORT-MODULE.

           GO TO PROCESS-FILE-CH-DATE.

      *=================================================================
       PROCESS-FILE-CH-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - L P T R
      *
      *=================================================================
      *    IN  : BR-REC
      *          TR1534-VPI WORKERS
      *          TR1534-FILE-OPTION-WORKERS
      *
      *    OUT : TR1534-VPI WORKERS
      *          UPGRADE-STAT   [BAD/GOOD]
      *
      *    DESC: PROCESS SOURCE TO TARGET OF GIVEN FILE.
      *
      ******************************************************************
       PROCESS-FILE-TR SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-TR = "N" )
              GO TO PROCESS-FILE-TR-EXIT.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    START TRFILE DATE LOOP.
      *    TRFILE-YYYYMM NEEDS TO START FROM THE BEGINNING DATE MINUS
      *    ONE MONTH SO TO ALLOW THE ADD LOGIC TO WORK PROPERLY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BEG-DATE-YYYYMM            TO TRFILE-YYYYMM.

           IF ( TRFILE-YYYYMM-MM = 01 )
              SUBTRACT 1                 FROM TRFILE-YYYYMM-YYYY
              MOVE 12                      TO TRFILE-YYYYMM-MM
           ELSE
              SUBTRACT 1                 FROM TRFILE-YYYYMM-MM.

      *=================================================================
       PROCESS-FILE-TR-DATE.

           ADD 01                          TO TRFILE-YYYYMM-MM.

           IF ( TRFILE-YYYYMM-MM > 12 )
              ADD  01                      TO TRFILE-YYYYMM-YYYY
              MOVE 01                      TO TRFILE-YYYYMM-MM.

           IF ( TRFILE-YYYYMM > END-DATE-YYYYMM )
              GO TO PROCESS-FILE-TR-EXIT.

      *- - - - - - - - - - - - - - - -{ SET TRFILE DATE - TRFILE-MMYY  }
           MOVE TRFILE-YYYYMM              TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY                  TO TRFILE-MMYY.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }

           MOVE "Y"                        TO  TRB-PATH-OVERRIDE
                                               TRC-PATH-OVERRIDE
                                               TRD-PATH-OVERRIDE
                                               TRI-PATH-OVERRIDE
                                               TRJ-PATH-OVERRIDE
                                               TRK-PATH-OVERRIDE
                                               TRL-PATH-OVERRIDE
                                               TRP-PATH-OVERRIDE
                                               TRS-PATH-OVERRIDE
                                               TRW-PATH-OVERRIDE
                                               TRX-PATH-OVERRIDE.

           MOVE "TR"                       TO TR1534-CURRENT-FILE.
           MOVE TRFILE-MMYY                TO TR1534-CURRENT-MMYY.

           MOVE TR1534-FILE-TYPE---LOCAL   TO TR1534-FILE-TYPE.
           MOVE ZEROES                     TO TR1534-RECORD-COUNTERS.

           PERFORM TR1534-LOAD-FILE-BEG-TIME.
           PERFORM TR1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE TR1534-FILE-ORDER---SOURCE TO TR1534-FILE-ORDER.

           MOVE SPACES                     TO TR1534-FILE-INPUT.
           STRING "ED/TR", TRFILE-MMYY
                  DELIMITED BY SIZE      INTO TR1534-FILE-INPUT.

           PERFORM TR1534-LOAD-FILE-PATH.
           MOVE TR1534-FILE-PATH           TO ACCESS-BUF       OTR-PATH.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              GO TO PROCESS-FILE-TR-DATE.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE TR1534-FILE-ORDER---TARGET TO TR1534-FILE-ORDER.

           MOVE "LP/TRBFILE"               TO TR1534-FILE-INPUT.
           PERFORM TR1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-TR = "Y")
              MOVE TR1534-FILE-PATH        TO TRB-PATH
              PERFORM OPEN-TRB1-FILE
           ELSE
              MOVE TR1534-FILE-PATH TO ATRB-PATH-BEG
                                       ATRB-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRB-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRB-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRB-PATH-GROUP 
                                            ATRB-DONE-PATH-GROUP
              PERFORM OPEN-ATRB-FILE.

           MOVE "LP/TRCFILE"               TO TR1534-FILE-INPUT.
           PERFORM TR1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-TR = "Y")
              MOVE TR1534-FILE-PATH        TO TRC-PATH
              PERFORM OPEN-TRC1-FILE
           ELSE
              MOVE TR1534-FILE-PATH TO ATRC-PATH-BEG
                                       ATRC-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRC-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRC-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRC-PATH-GROUP 
                                            ATRC-DONE-PATH-GROUP
              PERFORM OPEN-ATRC-FILE.

           MOVE "LP/TRDFILE"               TO TR1534-FILE-INPUT.
           PERFORM TR1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-TR = "Y")
              MOVE TR1534-FILE-PATH        TO TRD-PATH
              PERFORM OPEN-TRD1-FILE
           ELSE
              MOVE TR1534-FILE-PATH TO ATRD-PATH-BEG
                                       ATRD-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRD-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRD-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRD-PATH-GROUP 
                                            ATRD-DONE-PATH-GROUP
              PERFORM OPEN-ATRD-FILE.

           MOVE "LP/TRIFILE"               TO TR1534-FILE-INPUT.
           PERFORM TR1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-TR = "Y")
              MOVE TR1534-FILE-PATH        TO TRI-PATH
              PERFORM OPEN-TRI1-FILE
           ELSE
              MOVE TR1534-FILE-PATH TO ATRI-PATH-BEG
                                       ATRI-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRI-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRI-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRI-PATH-GROUP 
                                            ATRI-DONE-PATH-GROUP
              PERFORM OPEN-ATRI-FILE.

           MOVE "LP/TRJFILE"               TO TR1534-FILE-INPUT.
           PERFORM TR1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-TR = "Y")
              MOVE TR1534-FILE-PATH        TO TRJ-PATH
              PERFORM OPEN-TRJ1-FILE
           ELSE
              MOVE TR1534-FILE-PATH TO ATRJ-PATH-BEG
                                       ATRJ-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRJ-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRJ-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRJ-PATH-GROUP 
                                            ATRJ-DONE-PATH-GROUP
              PERFORM OPEN-ATRJ-FILE.
 
           MOVE "LP/TRKFILE"               TO TR1534-FILE-INPUT.
           PERFORM TR1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-TR = "Y")
              MOVE TR1534-FILE-PATH        TO TRK-PATH
              PERFORM OPEN-TRK1-FILE
           ELSE
              MOVE TR1534-FILE-PATH TO ATRK-PATH-BEG
                                       ATRK-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRK-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRK-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRK-PATH-GROUP 
                                            ATRK-DONE-PATH-GROUP
              PERFORM OPEN-ATRK-FILE.

           MOVE "LP/TRLFILE"               TO TR1534-FILE-INPUT.
           PERFORM TR1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-TR = "Y")
              MOVE TR1534-FILE-PATH        TO TRL-PATH
              PERFORM OPEN-TRL1-FILE
           ELSE
              MOVE TR1534-FILE-PATH TO ATRL-PATH-BEG
                                       ATRL-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRL-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRL-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRL-PATH-GROUP 
                                            ATRL-DONE-PATH-GROUP
              PERFORM OPEN-ATRL-FILE.

           MOVE "LP/TRPFILE"               TO TR1534-FILE-INPUT.
           PERFORM TR1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-TR = "Y")
              MOVE TR1534-FILE-PATH        TO TRP-PATH
              PERFORM OPEN-TRP1-FILE
           ELSE
              MOVE TR1534-FILE-PATH TO ATRP-PATH-BEG
                                       ATRP-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRP-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRP-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRP-PATH-GROUP 
                                            ATRP-DONE-PATH-GROUP
              PERFORM OPEN-ATRP-FILE.

           MOVE "LP/TRSFILE"               TO TR1534-FILE-INPUT.
           PERFORM TR1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-TR = "Y")
              MOVE TR1534-FILE-PATH        TO TRS-PATH
              PERFORM OPEN-TRS1-FILE
           ELSE
              MOVE TR1534-FILE-PATH TO ATRS-PATH-BEG
                                       ATRS-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRS-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRS-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRS-PATH-GROUP 
                                            ATRS-DONE-PATH-GROUP
              PERFORM OPEN-ATRS-FILE.

           MOVE "LP/TRWFILE"               TO TR1534-FILE-INPUT.
           PERFORM TR1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-TR = "Y")
               MOVE TR1534-FILE-PATH        TO TRW-PATH
              PERFORM OPEN-TRW1-FILE
           ELSE
              MOVE TR1534-FILE-PATH TO ATRW-PATH-BEG
                                       ATRW-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRW-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRW-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRW-PATH-GROUP 
                                            ATRW-DONE-PATH-GROUP
              PERFORM OPEN-ATRW-FILE.

           MOVE "LP/TRXFILE"               TO TR1534-FILE-INPUT.
           PERFORM TR1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-TR = "Y")
              MOVE TR1534-FILE-PATH        TO TRX-PATH
              PERFORM OPEN-TRX1-FILE
           ELSE
              MOVE TR1534-FILE-PATH TO ATRX-PATH-BEG
                                       ATRX-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ATRX-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ATRX-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ATRX-PATH-GROUP 
                                            ATRX-DONE-PATH-GROUP
              PERFORM OPEN-ATRX-FILE.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM MISC-TRACE-FILE-START.

      *- - - - - - - - - - - - - - - - - - -{ CHECK SOURCE FILE STATUS }
           IF ( FILE-OPTION-TR = "C" )
              PERFORM OPEN-OTR1-FILE
              PERFORM CLOSE-OTR1-FILE
              GO TO PROCESS-FILE-TR-EXIT.

           MOVE "TR"                       TO TR1534-CURRENT-FILE.

           MOVE LOW-VALUES                 TO OTR1-KEY.
           MOVE ZEROES                     TO OTR1-DAY
                                              OTR1-BRNO
                                              OTR1-CLASS.

           PERFORM OPEN-OTR1-FILE.
           PERFORM START-OTR1-FILE.

       PROCESS-FILE-TR-NEXT.

           PERFORM READ-OTR1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-TR-END.

           ADD  01  TO TR1534-RECORD-COUNTER-READ.
           MOVE "Y" TO TR1534-RECORDS-FOUND.
           PERFORM TR1534-VPI-DISPLAY.

           IF ( OTR1-CODE = "E" )
               ADD 1 TO TR1534-RECORD-COUNTER-SKIP
               GO TO PROCESS-FILE-TR-NEXT.
 
      *- - - - - - - - - - - - - - - - - - - - - - - - -{ VALID RECORD }
           IF ( OTR1-CODE = "B" )
              PERFORM UPGRADE-FILE-TRB
           ELSE
           IF ( OTR1-CODE = "C" )
              PERFORM UPGRADE-FILE-TRC
           ELSE
           IF ( OTR1-CODE = "D" )
              PERFORM UPGRADE-FILE-TRD
           ELSE
      * F - PAYMENTS WITH POOLID
      * G - PAYMENTS FOR SET POOL "SP"
      * P - STANDARD PAYMENTS
           IF ( OTR1-CODE = "F" ) OR
              ( OTR1-CODE = "G" ) OR
              ( OTR1-CODE = "P" ) 
              PERFORM UPGRADE-FILE-TRP
           ELSE
           IF ( OTR1-CODE = "I" )
              PERFORM UPGRADE-FILE-TRI
           ELSE
           IF ( OTR1-CODE = "J" )
              PERFORM UPGRADE-FILE-TRJ
           ELSE
           IF ( OTR1-CODE = "K" )
              PERFORM UPGRADE-FILE-TRK
           ELSE
           IF ( OTR1-CODE = "L" )
              PERFORM UPGRADE-FILE-TRL
           ELSE
           IF ( OTR1-CODE = "S" )
              PERFORM UPGRADE-FILE-TRS
           ELSE
           IF ( OTR1-CODE = "W" )
              PERFORM UPGRADE-FILE-TRW
           ELSE
           IF ( OTR1-CODE = "X" )
              PERFORM UPGRADE-FILE-TRX
           ELSE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              ADD 1 TO TR1534-RECORD-COUNTER-SKIP
              GO TO PROCESS-FILE-TR-NEXT.

           IF ( OTR1-CODE = "F" ) OR
              ( OTR1-CODE = "G" )
               ADD 1 TO TR1534-RECORD-COUNTER-SKIP-F-G
           ELSE
           IF ( IO-GOOD )
              ADD 1 TO TR1534-RECORD-COUNTER-WRITE.

           GO TO PROCESS-FILE-TR-NEXT.

       PROCESS-FILE-TR-END.

           PERFORM CLOSE-WK-FILE.
           PERFORM CLOSE-OTR1-FILE.
           IF (FILE-OPTION-TR = "Y")
              PERFORM CLOSE-TRB1-FILE
              PERFORM CLOSE-TRC1-FILE
              PERFORM CLOSE-TRD1-FILE
              PERFORM CLOSE-TRI1-FILE
              PERFORM CLOSE-TRJ1-FILE
              PERFORM CLOSE-TRK1-FILE
              PERFORM CLOSE-TRL1-FILE
              PERFORM CLOSE-TRP1-FILE
              PERFORM CLOSE-TRS1-FILE
              PERFORM CLOSE-TRW1-FILE
              PERFORM CLOSE-TRX1-FILE
           ELSE
              PERFORM CLOSE-ATRB-FILE
              PERFORM CLOSE-ATRC-FILE
              PERFORM CLOSE-ATRD-FILE
              PERFORM CLOSE-ATRI-FILE
              PERFORM CLOSE-ATRJ-FILE
              PERFORM CLOSE-ATRK-FILE
              PERFORM CLOSE-ATRL-FILE
              PERFORM CLOSE-ATRP-FILE
              PERFORM CLOSE-ATRS-FILE
              PERFORM CLOSE-ATRW-FILE
              PERFORM CLOSE-ATRX-FILE.

           PERFORM TR1534-CREATE-FILE-TOTALS.
           MOVE "TRFILE"                   TO HOLD-SORT-FILENAME.
           MOVE OTR-PATH                   TO HOLD-SORT-FILEPATH.
           MOVE TRB-PATH-OWNST             TO HOLD-SORT-STATE.
           MOVE TRB-PATH-OWNBR             TO HOLD-SORT-BRNO.
           PERFORM UPDATE-SORT-MODULE.


           GO TO PROCESS-FILE-TR-DATE.

       PROCESS-FILE-TR-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R B
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRB-REC   [ TRB-REC - BRANCH (BULK) TRANSFER]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       UPGRADE-FILE-TRB SECTION.

           IF FILE-OPTION-TR = "Y"
              PERFORM CLEAR-TRB-FILE
           ELSE
              PERFORM CLEAR-ATRB-FILE.
           
           MOVE OTB-BRNO            TO TB-BRNO ATB-BRNO.

           MOVE TRFILE-MMYY  TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMYY-YY TO DATE-YYMMDD-YY.
           MOVE OTB-DAY      TO DATE-YYMMDD-DD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.

           MOVE DATE-YYYYMMDD       TO TB-DATE     ATB-DATE.
           MOVE OTB-CLASS           TO TB-CLASS    ATB-CLASS.
           MOVE OTB-NUMBER          TO TB-ACCTNO   ATB-ACCTNO.
           MOVE OTB-LAWCODE         TO TB-LAWCODE  ATB-LAWCODE.
           MOVE OTB-LINE            TO TB-LINE     ATB-LINE.
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
               MOVE OTB-SSNO(SUB)    TO  TB-SSNO(SUB) ATB-SSNO(SUB)
               MOVE OTB-MAKERCD(SUB) TO  TB-MAKERCD(SUB)
                                        ATB-MAKERCD(SUB)
           END-PERFORM.
      
           MOVE OTB-REF-FRTO        TO TB-REF-FRTO ATB-REF-FRTO.
           MOVE OTB-REFBR           TO TB-REFBR    ATB-REFBR.

           MOVE OTB-PURCD           TO TB-PURCD    ATB-PURCD.
           MOVE OTB-SRCD            TO TB-SRCD     ATB-SRCD.
           MOVE OTB-COLLCD          TO TB-COLLCD   ATB-COLLCD.
           MOVE OTB-SERVACCT        TO TB-SERVACCT ATB-SERVACCT.
           MOVE OTB-LOANTYPE        TO TB-LOANTYPE ATB-LOANTYPE.

           MOVE OTB-LOANDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TB-LOANDATE ATB-LOANDATE.
      
           MOVE OTB-PROCEEDS        TO TB-PROCEEDS ATB-PROCEEDS.
           MOVE OTB-LNAMT           TO TB-LNAMT    ATB-LNAMT.
           MOVE OTB-FINCHG          TO TB-FINCHG   ATB-FINCHG.
           MOVE OTB-INTCHG          TO TB-INTCHG   ATB-INTCHG.
           MOVE OTB-SERCHG          TO TB-SERCHG   ATB-SERCHG.
           MOVE OTB-EXTCHG          TO TB-EXTCHG   ATB-EXTCHG.

           MOVE OTB-MAINTFEE        TO TB-MAINTFEE ATB-MAINTFEE.
           MOVE OTB-ORGTERM         TO TB-ORGTERM  ATB-ORGTERM.
      
           MOVE OTB-INSURED         TO TB-INSURED ATB-INSURED.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              MOVE 0                   TO TB-INSUNEARN(SUB)
                                         ATB-INSUNEARN(SUB)
           END-PERFORM.

           MOVE OTB-DLNO          TO TB-DLNO        ATB-DLNO.
           MOVE OTB-DLRV          TO TB-DLRV        ATB-DLRV.
           MOVE OTB-DLSPRV        TO TB-DLSPRV      ATB-DLSPRV.
           MOVE OTB-DLDISC        TO TB-DLDISC      ATB-DLDISC.
           MOVE OTB-DLPARV        TO TB-DLPARV      ATB-DLPARV.
           MOVE OTB-DLRECOURSE    TO TB-DLRECOURSE  ATB-DLRECOURSE.

           MOVE OTB-DLPARTRCAMT   TO TB-DLPARTRCAMT ATB-DLPARTRCAMT.
           MOVE OTB-PURBAL        TO TB-PURBAL      ATB-PURBAL.
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6 
             MOVE OTB-UNEARN(SUB)   TO TB-UNEARN(SUB) ATB-UNEARN(SUB)
           END-PERFORM.

      *CHANGED TB-REF-ACCTNO TO BE THE 8 DIGIT ACCTNO WITHOUT 01 SEQUENCE
           MOVE OTB-REF-ACCTNO        TO TEST-ACCTNO.
           MOVE TEST-NUMBER           TO TB-REF-ACCTNO ATB-REF-ACCTNO.
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               MOVE OTB-DLRVPRIN(SUB)    TO TB-DLRVPRIN(SUB)
                                           ATB-DLRVPRIN(SUB)
               MOVE OTB-DLRVCHGS(SUB)    TO TB-DLRVCHGS(SUB)
                                           ATB-DLRVCHGS(SUB)
           END-PERFORM.

           MOVE OTB-DLPARVPAID      TO TB-DLPARVPAID ATB-DLPARVPAID.
           MOVE OTB-DLPARVHELD      TO TB-DLPARVHELD ATB-DLPARVHELD.
           MOVE OTB-USERID          TO TB-USERID     ATB-USERID.

           MOVE OTB-POSTDATE       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO TB-POSTDATE    ATB-POSTDATE.

           MOVE OTB-POSTTIME       TO TB-POSTTIME    ATB-POSTTIME.
           MOVE OTB-PLCD           TO TB-PLCD        ATB-PLCD.
           MOVE OTB-PLTYPE         TO TB-PLTYPE      ATB-PLTYPE.
           MOVE OTB-REPOCD         TO TB-REPOCD      ATB-REPOCD.
           MOVE OTB-ACTIONCD2      TO TB-ACTIONCD2   ATB-ACTIONCD2.

           IF (FILE-OPTION-TR = "Y")
              PERFORM WRITE-TRB1-FILE
           ELSE
              ADD 1 TO ATRB-KEY
              ACCEPT ATB-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ATB-CR
              MOVE X"0A"              TO ATB-LF
              PERFORM WRITE-ATRB-FILE.

           IF ( IO-BAD )                 
              ADD 1               TO TR1534-RECORD-COUNTER-SKIP
              MOVE "TRB"          TO TR1534-CURRENT-FILE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              GO TO IO-ERROR.

       UPGRADE-FILE-TRB-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - C H
      *
      *=================================================================
      *    IN  : OCH-REC
      *    OUT : CH-REC
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       UPGRADE-FILE-CH SECTION.

           IF FILE-OPTION-TR = "Y"
              PERFORM CLEAR-CH-FILE
           ELSE
              PERFORM CLEAR-ACH-FILE.

           MOVE BR-NO                   TO CH-BRNO   ACH-BRNO.

           MOVE CHFILE-MMYY  TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMYY-YY TO DATE-YYMMDD-YY.
           MOVE OCH-DAY      TO DATE-YYMMDD-DD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD           TO CH-DATE    ACH-DATE.

           MOVE OCH-CODE                TO CH-CODE    ACH-CODE.
           MOVE OCH-USERID              TO CH-USERID  ACH-USERID.

           MOVE OCH-HR                  TO WSS-CH-HR.
           MOVE OCH-MIN                 TO WSS-CH-MIN.
           MOVE OCH-SEC                 TO WSS-CH-SEC.
           MOVE WSS-CH-TIME             TO CH-TIME    ACH-TIME.

      * A15 HAD NO SEQUENCE, FORCE TO 1
           MOVE 01                      TO CH-SEQ     ACH-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ACCTNO/SSNO/TYPE DEFAULTED TO ZEROES.
      *    ACCTNO/SSNO/TYPE WILL BE SET BELOW BASED ON CODE LAYOUT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      * ONLY CERTAIN CODES HAVE VALID DESCRIPT
           IF OCH-CODE = "B" OR "T" OR "Z" OR "M" OR "C"
              MOVE OCH-DESCRIPT     TO CH-DESCRIPT ACH-DESCRIPT
           ELSE
              MOVE SPACES           TO CH-DESCRIPT ACH-DESCRIPT.
      *    IF OCH-DESCRIPT NOT ALPHABETIC
      *       IF OCH-CODE = "C"
      *           MOVE "CONFIDENTIAL" TO CH-DESCRIPT ACH-DESCRIPT
      *       ELSE
      *       IF OCH-CODE = "T"
      *          MOVE "COLLATERAL TRAILER" TO CH-DESCRIPT ACH-DESCRIPT
      *       ELSE
      *       IF OCH-CODE = "Z"
      *          MOVE "DEFICIENCY TRAILER" TO CH-DESCRIPT ACH-DESCRIPT
      *       ELSE
      *       IF OCH-CODE = "M"
      *          MOVE "MAINTENANCE" TO CH-DESCRIPT ACH-DESCRIPT.
                     

           MOVE ZEROES                  TO CH-ACCTNO   ACH-ACCTNO.
           MOVE ZEROES                  TO CH-SSNO     ACH-SSNO.
           MOVE ZEROES                  TO CH-TYPE     ACH-TYPE.
           MOVE SPACES                  TO CH-ORG-CHAR ACH-ORG-CHAR
                                           CH-NEW-CHAR ACH-NEW-CHAR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "A"  (DEALER NUMBER  )
      *    CH-CODE = "S"  (SOCIAL SECURITY)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "A" OR "S" )
              MOVE OCH-OLD-SSNO       TO CH-ORG-SSNO ACH-ORG-SSNO
              MOVE OCH-NEW-SSNO       TO CH-NEW-SSNO ACH-NEW-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "B"  (BANKRUPTCY) USING REDEFS BUT NOT THE CH-M-SSN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "B" )
              MOVE OCH-CREDIT-ACCTNO    TO TEST-ACCTNO
              MOVE TEST-NUMBER          TO CH-ACCTNO   ACH-ACCTNO
              IF ( OCH-DESCRIPT = "BANKRUPT DATE" )
                 MOVE OCH-ORG-DATE TO CH-ORG-DATE  ACH-ORG-DATE
                 MOVE OCH-NEW-DATE TO CH-NEW-DATE  ACH-NEW-DATE
              ELSE
              IF ( OCH-DESCRIPT = "REAFF EFFDATE" )
                 MOVE OCH-ORG-DATE TO CH-ORG-DATE  ACH-ORG-DATE
                 MOVE OCH-NEW-DATE TO CH-NEW-DATE  ACH-NEW-DATE
              ELSE
              IF ( OCH-DESCRIPT = "REAFF SYSDATE" )
                 MOVE OCH-ORG-DATE TO CH-ORG-DATE  ACH-ORG-DATE
                 MOVE OCH-NEW-DATE TO CH-NEW-DATE  ACH-NEW-DATE
              ELSE
              IF ( OCH-DESCRIPT = "REAFF BALANCE" )
                 MOVE OCH-ORG-6DEC2 TO CH-ORG-6DEC2  ACH-ORG-6DEC2
                 MOVE OCH-NEW-6DEC2 TO CH-NEW-6DEC2  ACH-NEW-6DEC2
              ELSE
              IF ( OCH-DESCRIPT = "REAFF PAYAMT" )
                 MOVE OCH-ORG-6DEC2 TO CH-ORG-6DEC2  ACH-ORG-6DEC2
                 MOVE OCH-NEW-6DEC2 TO CH-NEW-6DEC2  ACH-NEW-6DEC2
              ELSE
              IF ( OCH-DESCRIPT = "REAFF NUMBER PAYS" )
                 MOVE OCH-ORG-NODEC TO CH-ORG-NODEC  ACH-ORG-NODEC
                 MOVE OCH-NEW-NODEC TO CH-NEW-NODEC  ACH-NEW-NODEC
              ELSE
              IF ( OCH-DESCRIPT = "REAFF INTRATE" )
                 MOVE OCH-ORG-RATE TO CH-ORG-RATE  ACH-ORG-RATE
                 MOVE OCH-NEW-RATE TO CH-NEW-RATE  ACH-NEW-RATE
              ELSE
              IF ( OCH-DESCRIPT = "REAFF RESCIND DATE" )
                 MOVE OCH-ORG-DATE TO CH-ORG-DATE  ACH-ORG-DATE
                 MOVE OCH-NEW-DATE TO CH-NEW-DATE  ACH-NEW-DATE
              ELSE
              IF ( OCH-DESCRIPT = "REAFF REAFF TYPE" )
                 MOVE OCH-ORG-CHAR TO CH-ORG-CHAR  ACH-ORG-CHAR
                 MOVE OCH-NEW-CHAR TO CH-NEW-CHAR  ACH-NEW-CHAR
              ELSE
              IF ( OCH-DESCRIPT = "DISCHARGE EFFDATE" )
                 MOVE OCH-ORG-DATE TO CH-ORG-DATE  ACH-ORG-DATE
                 MOVE OCH-NEW-DATE TO CH-NEW-DATE  ACH-NEW-DATE
              ELSE
              IF ( OCH-DESCRIPT = "DISCHARGE SYSDATE" )
                 MOVE OCH-ORG-DATE TO CH-ORG-DATE  ACH-ORG-DATE
                 MOVE OCH-NEW-DATE TO CH-NEW-DATE  ACH-NEW-DATE
              ELSE
                 MOVE OCH-ORG-CHAR TO CH-ORG-CHAR  ACH-ORG-CHAR
                 MOVE OCH-NEW-CHAR TO CH-NEW-CHAR  ACH-NEW-CHAR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "C"  (CONFIDENTIAL FLAG CHANGE)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "C" )
              MOVE OCH-CONFID-ACCTNO    TO TEST-ACCTNO
              MOVE TEST-NUMBER          TO CH-ACCTNO   ACH-ACCTNO
              MOVE OCH-OLD-CONFIDENT    TO CH-ORG-CHAR ACH-ORG-CHAR
              MOVE OCH-NEW-CONFIDENT    TO CH-NEW-CHAR ACH-NEW-CHAR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "D"  (DEBT-AID    )
      *    CH-CODE = "F"  (CONFIRMATION)  
      *    CH-CODE = "H"  (CREDIT LIMIT)  
      *    CH-CODE = "L"  (LEGAL       )
      *    CH-CODE = "X"  (REPOSSIONS  )
      *    CH-CODE = "1"  (GROSS INCOME)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "D" OR "F" OR "H" OR "L" OR "X" OR "1" )
              MOVE OCH-CREDIT-ACCTNO  TO TEST-ACCTNO
              MOVE TEST-NUMBER        TO CH-ACCTNO    ACH-ACCTNO
              MOVE OCH-OLD-CREDIT     TO CH-ORG-NODEC ACH-ORG-NODEC
              MOVE OCH-NEW-CREDIT     TO CH-NEW-NODEC ACH-NEW-NODEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "E"  (FROZEN ACCOUNT; ACCOUNT HAS BEEN LOCKED)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "E" )
              MOVE OCH-FROZEN-ACCTNO  TO TEST-ACCTNO
              MOVE TEST-NUMBER        TO CH-ACCTNO   ACH-ACCTNO
              MOVE OCH-OLD-FROZEN     TO CH-ORG-CHAR ACH-ORG-CHAR
              MOVE OCH-NEW-FROZEN     TO CH-NEW-CHAR ACH-NEW-CHAR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "K"  (TRW; CREDIT BUREAU CHANGES)
      *    CH-CODE = "P"  (P&L REASON CODE CHANGES)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "K" OR "P" )
              MOVE OCH-TRW-ACCTNO     TO TEST-ACCTNO
              MOVE TEST-NUMBER        TO CH-ACCTNO   ACH-ACCTNO
              MOVE OCH-OLD-TRW        TO CH-ORG-CHAR ACH-ORG-CHAR
              MOVE OCH-NEW-TRW        TO CH-NEW-CHAR ACH-NEW-CHAR
              IF ( OCH-CODE = "P" )
                 MOVE ZEROES          TO CH-TYPE     ACH-TYPE
              ELSE
                 MOVE OCH-TRW-TYPE    TO CH-TYPE     ACH-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "M"  (MAINTENANCE CHANGES, BORROWER)
      *    CH-CODE = "Q"  (INVESTOR CHANGES)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "M" OR "Q" )
              MOVE OCH-M-SSN          TO CH-SSNO     ACH-SSNO
              MOVE OCH-ORG-CHAR       TO CH-ORG-CHAR ACH-ORG-CHAR
              MOVE OCH-NEW-CHAR       TO CH-NEW-CHAR ACH-NEW-CHAR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "N"  (CHANGE OF LOAN ORIGIN)      
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "N" )
              MOVE OCH-LOAN-ORIGIN-ACCTNO TO TEST-ACCTNO
              MOVE TEST-NUMBER         TO CH-ACCTNO   ACH-ACCTNO
              MOVE OCH-OLD-LOAN-ORIGIN TO CH-ORG-CHAR ACH-ORG-CHAR
              MOVE OCH-NEW-LOAN-ORIGIN TO CH-NEW-CHAR ACH-NEW-CHAR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "O"  CHANGE ESCROW FG FROM "Y" TO "I"(INACTIVE)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "O" )
              MOVE OCH-ESCROW-ACCTNO  TO TEST-ACCTNO
              MOVE TEST-NUMBER        TO CH-ACCTNO   ACH-ACCTNO
              MOVE OCH-OLD-ESCROW-FG  TO CH-ORG-CHAR ACH-ORG-CHAR
              MOVE OCH-NEW-ESCROW-FG  TO CH-NEW-CHAR ACH-NEW-CHAR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "R"   (NOTE MASTER CHANGES)
      *    CH-CODE = "T"   (COLLATERAL TRAILER CHANGES)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "R" OR "T" )
              MOVE OCH-ORG-CHAR       TO CH-ORG-CHAR ACH-ORG-CHAR
              MOVE OCH-NEW-CHAR       TO CH-NEW-CHAR ACH-NEW-CHAR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "Y"  (ALLOTCD/LBOX  )
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "Y" )
              MOVE OCH-CREDIT-ACCTNO TO TEST-ACCTNO
              MOVE TEST-NUMBER       TO CH-ACCTNO      ACH-ACCTNO
              MOVE OCH-OLD-ALLOTCD   TO CH-ORG-ALLOTCD ACH-ORG-ALLOTCD
              MOVE OCH-NEW-ALLOTCD   TO CH-NEW-ALLOTCD ACH-NEW-ALLOTCD
              MOVE OCH-OLD-LBOX      TO CH-ORG-LBOX    ACH-ORG-LBOX
              MOVE OCH-NEW-LBOX      TO CH-NEW-LBOX    ACH-NEW-LBOX.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "Z"   (DEFICIENCY TRAILER CHANGES)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "Z" )
              MOVE OCH-CREDIT-ACCTNO  TO TEST-ACCTNO
              MOVE TEST-NUMBER        TO CH-ACCTNO   ACH-ACCTNO
              MOVE OCH-ORG-CHAR       TO CH-ORG-CHAR ACH-ORG-CHAR
              MOVE OCH-NEW-CHAR       TO CH-NEW-CHAR ACH-NEW-CHAR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CH-CODE = "2"  CHANGE TDR1 (DEBT RESTRUCTURE)               
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OCH-CODE = "2" )
              MOVE OCH-TDRS-ACCTNO    TO TEST-ACCTNO
              MOVE TEST-NUMBER        TO CH-ACCTNO       ACH-ACCTNO
              MOVE OCH-TDR-1-2        TO CH-TDR-1-2      ACH-TDR-1-2
              MOVE OCH-OLD-TDR        TO CH-ORG-TDR      ACH-ORG-TDR
              MOVE OCH-OLD-TDR-DATE   TO CH-ORG-TDR-DATE
                                        ACH-ORG-TDR-DATE
              MOVE OCH-NEW-TDR        TO CH-NEW-TDR      ACH-NEW-TDR
              MOVE OCH-NEW-TDR-DATE   TO CH-NEW-TDR-DATE
                                        ACH-NEW-TDR-DATE
              MOVE OCH-TDRS-SOURCE    TO CH-TDRS-SOURCE
                                        ACH-TDRS-SOURCE.

       UPGRADE-FILE-CH-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R C
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRC-REC   [ TRC-REC - CHECKS/CASH ]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       UPGRADE-FILE-TRC SECTION.

           IF FILE-OPTION-TR = "Y"
              PERFORM CLEAR-TRC-FILE
           ELSE
              PERFORM CLEAR-ATRC-FILE.
 
           MOVE OTC-BRNO            TO TC-BRNO  ATC-BRNO.

           MOVE TRFILE-MMYY  TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMYY-YY TO DATE-YYMMDD-YY.
           MOVE OTC-DAY      TO DATE-YYMMDD-DD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.

           MOVE DATE-YYYYMMDD       TO TC-KEY-DATE ATC-KEY-DATE.
           MOVE OTC-CKNO            TO TC-CKNO     ATC-CKNO.
           MOVE OTC-CLASS           TO TC-CLASS    ATC-CLASS.
           MOVE OTC-SEQ             TO TC-SEQ      ATC-SEQ.
      
           MOVE OTC-TYPE            TO TC-TYPE     ATC-TYPE.
      
           MOVE OTC-DATE            TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TC-DATE     ATC-DATE.

           MOVE OTC-AMT             TO TC-AMT      ATC-AMT.

           MOVE OTC-PAYEE           TO TC-PAYEE    ATC-PAYEE.
           MOVE OTC-REF             TO TC-REF      ATC-REF.
           MOVE OTC-REASON          TO TC-REASON   ATC-REASON.
           
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               MOVE OTC-GLNO(SUB)    TO TC-GLNO(SUB)  ATC-GLNO(SUB)
               MOVE OTC-GLAMT(SUB)   TO TC-GLAMT(SUB) ATC-GLAMT(SUB)
           END-PERFORM.

           MOVE OTC-VOID            TO TC-VOID     ATC-VOID.

           MOVE OTC-ACCTNO          TO TEST-ACCTNO.
           MOVE TEST-NUMBER         TO TC-ACCTNO   ATC-ACCTNO.

           MOVE OTC-PAID            TO TC-PAID     ATC-PAID.
           MOVE OTC-PGM             TO TC-PGM      ATC-PGM.
      * CLEANSE
           IF OTC-DLRVNO NOT NUMERIC
              MOVE 0                TO TC-DLRVNO ATC-DLRVNO
           ELSE
              MOVE OTC-DLRVNO       TO TC-DLRVNO   ATC-DLRVNO.
           MOVE OTC-SPRCLASS        TO TC-SPRCLASS ATC-SPRCLASS.
           MOVE OTC-DLNO            TO TC-DLNO     ATC-DLNO.
           MOVE OTC-USERID          TO TC-USERID   ATC-USERID.
           MOVE OTC-POSTDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TC-POSTDATE ATC-POSTDATE.
           MOVE OTC-POSTTIME        TO TC-POSTTIME ATC-POSTTIME.

           IF OTC-TILLNO = "1"
              MOVE 1  TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "2"
              MOVE 2  TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "3"
              MOVE 3  TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "4"
              MOVE 4  TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "5"
              MOVE 5  TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "6"
              MOVE 6  TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "7"
              MOVE 7  TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "8"
              MOVE 8  TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "9"
              MOVE 9  TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "A"
              MOVE 10 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "B"
              MOVE 11 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "C"
              MOVE 12 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "D"
              MOVE 13 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "E"
              MOVE 14 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "F"
              MOVE 15 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "G"
              MOVE 16 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "H"
              MOVE 17 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "I"
              MOVE 18 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "J"
              MOVE 19 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "K"
              MOVE 20 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "L"
              MOVE 21 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "M"
              MOVE 22 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "N"
              MOVE 23 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "O"
              MOVE 24 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "P"
              MOVE 25 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "Q"
              MOVE 26 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "R"
              MOVE 27 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "S"
              MOVE 28 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "T"
              MOVE 29 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "U"
              MOVE 30 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "V"
              MOVE 31 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "W"
              MOVE 32 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "X"
              MOVE 33 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "Y"
              MOVE 34 TO TC-TILLNO ATC-TILLNO
           ELSE
           IF OTC-TILLNO = "Z"
              MOVE 35 TO TC-TILLNO ATC-TILLNO.

           MOVE OTC-ADR1            TO TC-ADR1   ATC-ADR1.
           MOVE SPACES              TO TC-ADR2   ATC-ADR2.
           MOVE SPACES              TO TC-CITY   ATC-CITY.
           MOVE SPACES              TO TC-STATE  ATC-STATE.
           MOVE SPACES              TO TC-ZIP    ATC-ZIP.
      
           MOVE OTC-PAYEE-NO        TO TC-PAYEE-NO ATC-PAYEE-NO.
           MOVE OTC-ACH-FG          TO TC-ACH-FG   ATC-ACH-FG.
           MOVE OTC-EXTRACTED-FG    TO TC-EXTRACT-FG   ATC-EXTRACT-FG.
           MOVE OTC-DEBIT-REFCD     TO TC-DEBIT-REFCD  ATC-DEBIT-REFCD.

           IF (FILE-OPTION-TR = "Y")
              PERFORM WRITE-TRC1-FILE
           ELSE
              ADD 1 TO ATRC-KEY
              ACCEPT ATC-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ATC-CR
              MOVE X"0A"              TO ATC-LF
              PERFORM WRITE-ATRC-FILE.

           IF ( IO-BAD )                 
              ADD 1               TO TR1534-RECORD-COUNTER-SKIP
              MOVE "TRC"          TO TR1534-CURRENT-FILE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              GO TO IO-ERROR.

       UPGRADE-FILE-TRC-EXIT.           
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R D
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRD-REC   [ TRD-REC - DEPOSIT ]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       UPGRADE-FILE-TRD SECTION.

           IF FILE-OPTION-TR = "Y"
              PERFORM CLEAR-TRD-FILE
           ELSE
              PERFORM CLEAR-ATRD-FILE.

           MOVE OTD-BRNO            TO TD-BRNO    ATD-BRNO.

           MOVE TRFILE-MMYY  TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMYY-YY TO DATE-YYMMDD-YY.
           MOVE OTD-DAY      TO DATE-YYMMDD-DD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.

           MOVE DATE-YYYYMMDD       TO TD-DATE    ATD-DATE.
      
           MOVE OTD-DEPOSIT         TO TD-DEPOSIT ATD-DEPOSIT.
           MOVE OTD-CHECK-DEPOSIT   TO TD-CHECK-DEPOSIT
                                      ATD-CHECK-DEPOSIT.
           MOVE OTD-BANK            TO TD-BANK    ATD-BANK.
           MOVE OTD-CASH            TO TD-CASH    ATD-CASH.
           MOVE OTD-GLFG            TO TD-GLFG    ATD-GLFG.
           MOVE OTD-GLOVERSHORT     TO TD-GLOVERSHORT ATD-GLOVERSHORT.

           MOVE OTD-OVERSHORT       TO TD-OVERSHORT   ATD-OVERSHORT.
           MOVE OTD-USERID          TO TD-USERID      ATD-USERID.

           MOVE OTD-POSTDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TD-POSTDATE    ATD-POSTDATE.  

           MOVE OTD-POSTTIME        TO TD-POSTTIME    ATD-POSTTIME.
 
           IF (FILE-OPTION-TR = "Y")
              PERFORM WRITE-TRD1-FILE
           ELSE
              ADD 1 TO ATRD-KEY
              ACCEPT ATD-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ATD-CR
              MOVE X"0A"              TO ATD-LF
              PERFORM WRITE-ATRD-FILE.

           IF ( IO-BAD )
              ADD 1               TO TR1534-RECORD-COUNTER-SKIP
              MOVE "TRD"          TO TR1534-CURRENT-FILE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              GO TO IO-ERROR.

       UPGRADE-FILE-TRD-EXIT.          
           EXIT.


      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R I
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRI-REC   [ TRI-REC - INSURANCE CLAIMS ]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       UPGRADE-FILE-TRI SECTION.

           IF FILE-OPTION-TR = "Y"
              PERFORM CLEAR-TRI-FILE
           ELSE
              PERFORM CLEAR-ATRI-FILE.

           MOVE TRFILE-MMYY  TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMYY-YY TO DATE-YYMMDD-YY.
           MOVE OTI-DAY      TO DATE-YYMMDD-DD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.

           MOVE OTI-BRNO            TO TI-BRNO        ATI-BRNO.
           MOVE DATE-YYYYMMDD       TO TI-KEY-DATE    ATI-KEY-DATE.
           MOVE OTI-SEQ             TO TI-SEQ         ATI-SEQ.
      
           MOVE OTI-WAIS            TO TI-WAIS        ATI-WAIS.
           MOVE OTI-TYPE            TO TI-TYPE        ATI-TYPE.
           MOVE OTI-AMOUNT          TO TI-AMOUNT      ATI-AMOUNT.
           MOVE OTI-CLAIM-NUMBER    TO TI-CLAIM-NUMBER ATI-CLAIM-NUMBER.
           MOVE OTI-LNAME           TO TI-LNAME       ATI-LNAME.
           MOVE OTI-FNAME           TO TI-FNAME       ATI-FNAME.

      * CLEANSE
           IF OTI-DATE NOT NUMERIC OR
               OTI-DATE = 020202 OR 
                OTI-DATE = 202020
              MOVE 0 TO OTI-DATE.

           MOVE OTI-DATE            TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TI-DATE        ATI-DATE.

           MOVE OTI-DRAFT           TO TI-DRAFT       ATI-DRAFT.
           MOVE OTI-PRODUCER        TO TI-PRODUCER    ATI-PRODUCER.

           MOVE OTI-USERID          TO TI-USERID      ATI-USERID.
           MOVE OTI-POSTTIME        TO TI-POSTTIME    ATI-POSTTIME.

      * CLEANSE
           IF OTI-POSTDATE NOT NUMERIC OR
               OTI-POSTDATE = 020202 OR 
                OTI-POSTDATE = 202020
              MOVE 0 TO OTI-POSTDATE.
           MOVE OTI-POSTDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TI-POSTDATE    ATI-POSTDATE.

           MOVE OTI-INSCOMP         TO TI-INSCOMP     ATI-INSCOMP.
           IF OTI-DAYS-PAID NOT NUMERIC
              MOVE 0                TO TI-DAYS-PAID   ATI-DAYS-PAID
           ELSE
              MOVE OTI-DAYS-PAID    TO TI-DAYS-PAID   ATI-DAYS-PAID.
           MOVE OTI-PMT-TYPE        TO TI-PMT-TYPE    ATI-PMT-TYPE.

      * BELIEVE FROM & TO DATE ARE STORED AS YYMMDD IN A15!
      * EXAMPLE: BR 18 DATE 1019; DAY 31, AMT $140.17 FROM 190923 TO 191021
      * CLEANSE
           IF OTI-FROM-DATE NOT NUMERIC OR
               OTI-FROM-DATE = 020202 OR 
                OTI-FROM-DATE = 202020
              MOVE 0 TO OTI-FROM-DATE.
           MOVE OTI-FROM-DATE       TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TI-FROM-DATE   ATI-FROM-DATE.

      * CLEANSE
           IF OTI-TO-DATE NOT NUMERIC OR
               OTI-TO-DATE = 020202 OR 
                OTI-TO-DATE = 202020
              MOVE 0 TO OTI-TO-DATE.
           MOVE OTI-TO-DATE         TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TI-TO-DATE     ATI-TO-DATE.

           IF (FILE-OPTION-TR = "Y")
              PERFORM WRITE-TRI1-FILE
           ELSE
              ADD 1 TO ATRI-KEY
              ACCEPT ATI-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ATI-CR
              MOVE X"0A"              TO ATI-LF
              PERFORM WRITE-ATRI-FILE.

           IF ( IO-BAD )
              ADD 1               TO TR1534-RECORD-COUNTER-SKIP
              MOVE "TRI"          TO TR1534-CURRENT-FILE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              GO TO IO-ERROR.

       UPGRADE-FILE-TRI-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R J
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRJ-REC   [ TRJ-REC - CLASS TRANSFER (WITHIN ONE BRANCH)]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       UPGRADE-FILE-TRJ SECTION.

           IF FILE-OPTION-TR = "Y"
              PERFORM CLEAR-TRJ-FILE
           ELSE
              PERFORM CLEAR-ATRJ-FILE.

           MOVE TRFILE-MMYY  TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMYY-YY TO DATE-YYMMDD-YY.
           MOVE OTJ-DAY      TO DATE-YYMMDD-DD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.

           MOVE OTJ-BRNO            TO TJ-BRNO      ATJ-BRNO.
           MOVE DATE-YYYYMMDD       TO TJ-DATE      ATJ-DATE.
           MOVE OTJ-CLASS           TO TJ-CLASS     ATJ-CLASS.
           MOVE OTJ-NUMBER          TO TJ-ACCTNO    ATJ-ACCTNO.
           MOVE OTJ-LAWCODE         TO TJ-LAWCODE   ATJ-LAWCODE.
           MOVE OTJ-LINE            TO TJ-LINE      ATJ-LINE.
 
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
               MOVE OTJ-SSNO(SUB)    TO TJ-SSNO(SUB) ATJ-SSNO(SUB)
               MOVE OTJ-MAKERCD(SUB) TO TJ-MAKERCD(SUB) ATJ-MAKERCD(SUB)
           END-PERFORM.

           MOVE OTJ-REFNO           TO TJ-REFNO    ATJ-REFNO.
           MOVE OTJ-REF-FRLC        TO TJ-REF-FRLC  TJ-FRLC
                                      ATJ-REF-FRLC ATJ-FRLC.
           MOVE OTJ-REF-TOLC        TO TJ-REF-TOLC  TJ-TOLC
                                      ATJ-REF-TOLC ATJ-TOLC.
           
           MOVE OTJ-PURCD           TO TJ-PURCD      ATJ-PURCD.
           MOVE OTJ-SRCD            TO TJ-SRCD       ATJ-SRCD.
           MOVE OTJ-COLLCD          TO TJ-COLLCD     ATJ-COLLCD.
           MOVE OTJ-SERVACCT        TO TJ-SERVACCT   ATJ-SERVACCT.
           MOVE OTJ-LOANTYPE        TO TJ-LOANTYPE   ATJ-LOANTYPE.

           MOVE OTJ-LOANDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TJ-LOANDATE   ATJ-LOANDATE.

           MOVE OTJ-PROCEEDS        TO TJ-PROCEEDS   ATJ-PROCEEDS.
           MOVE OTJ-LNAMT           TO TJ-LNAMT      ATJ-LNAMT.
           MOVE OTJ-FINCHG          TO TJ-FINCHG     ATJ-FINCHG.
           MOVE OTJ-INTCHG          TO TJ-INTCHG     ATJ-INTCHG.
           MOVE OTJ-SERCHG          TO TJ-SERCHG     ATJ-SERCHG.
           MOVE OTJ-EXTCHG          TO TJ-EXTCHG     ATJ-EXTCHG.
           MOVE OTJ-MAINTFEE        TO TJ-MAINTFEE   ATJ-MAINTFEE.
           MOVE OTJ-ORGTERM         TO TJ-ORGTERM    ATJ-ORGTERM.

           MOVE OTJ-INSURED         TO TJ-INSURED    ATJ-INSURED.
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE 0               TO TJ-INSUNEARN(SUB)
                                      ATJ-INSUNEARN(SUB)
           END-PERFORM.

           MOVE OTJ-DLNO            TO TJ-DLNO       ATJ-DLNO.
           MOVE OTJ-DLRV            TO TJ-DLRV       ATJ-DLRV.
           MOVE OTJ-DLSPRV          TO TJ-DLSPRV     ATJ-DLSPRV.
           MOVE OTJ-DLDISC          TO TJ-DLDISC     ATJ-DLDISC.
           MOVE OTJ-DLPARV          TO TJ-DLPARV     ATJ-DLPARV.
           MOVE OTJ-DLRECOURSE      TO TJ-DLRECOURSE ATJ-DLRECOURSE.
           MOVE OTJ-DLPARTRCAMT     TO TJ-DLPARTRCAMT ATJ-DLPARTRCAMT.
           MOVE OTJ-PURBAL          TO TJ-PURBAL     ATJ-PURBAL.
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             MOVE OTJ-UNEARN(SUB)   TO TJ-UNEARN(SUB) ATJ-UNEARN(SUB)
           END-PERFORM.
      
      *CHANGED TJ-PRLNNO TO BE THE 8 DIGIT ACCTNO WITHOUT 01 SEQUENCE
           MOVE OTJ-PRLNNO            TO TEST-ACCTNO.
           MOVE TEST-NUMBER           TO TJ-PRLNNO ATJ-PRLNNO.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               MOVE OTJ-DLRVPRIN(SUB) TO TJ-DLRVPRIN(SUB)
                                        ATJ-DLRVPRIN(SUB)
               MOVE OTJ-DLRVCHGS(SUB) TO TJ-DLRVCHGS(SUB)
                                        ATJ-DLRVCHGS(SUB)
           END-PERFORM.

           MOVE OTJ-DLPARVPAID      TO TJ-DLPARVPAID ATJ-DLPARVPAID.
           MOVE OTJ-DLPARVHELD      TO TJ-DLPARVHELD ATJ-DLPARVHELD.
           MOVE OTJ-USERID          TO TJ-USERID     ATJ-USERID.

           MOVE OTJ-POSTDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TJ-POSTDATE ATJ-POSTDATE.

           MOVE OTJ-POSTTIME        TO TJ-POSTTIME ATJ-POSTTIME.
           MOVE OTJ-PLCD            TO TJ-PLCD     ATJ-PLCD.
           MOVE OTJ-PLTYPE          TO TJ-PLTYPE   ATJ-PLTYPE.
           MOVE OTJ-REPOCD          TO TJ-REPOCD   ATJ-REPOCD.

           IF (FILE-OPTION-TR = "Y")
              PERFORM WRITE-TRJ1-FILE
           ELSE
              ADD 1 TO ATRJ-KEY
              ACCEPT ATJ-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ATJ-CR
              MOVE X"0A"              TO ATJ-LF
              PERFORM WRITE-ATRJ-FILE.

           IF ( IO-BAD )
              ADD 1               TO TR1534-RECORD-COUNTER-SKIP
              MOVE "TRJ"          TO TR1534-CURRENT-FILE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              GO TO IO-ERROR.

       UPGRADE-FILE-TRJ-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R K
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRK-REC   [ TRK-REC - CONVERSIONS ]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       UPGRADE-FILE-TRK SECTION.

           IF FILE-OPTION-TR = "Y"
              PERFORM CLEAR-TRK-FILE
           ELSE
              PERFORM CLEAR-ATRK-FILE.

           MOVE TRFILE-MMYY  TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMYY-YY TO DATE-YYMMDD-YY.
           MOVE OTK-DAY      TO DATE-YYMMDD-DD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.

           MOVE OTK-BRNO            TO TK-BRNO    ATK-BRNO.
           MOVE DATE-YYYYMMDD       TO TK-DATE    ATK-DATE.
           MOVE OTK-CLASS           TO TK-CLASS   ATK-CLASS.
           MOVE OTK-NUMBER          TO TK-ACCTNO  ATK-ACCTNO.
           MOVE OTK-LAWCODE         TO TK-LAWCODE ATK-LAWCODE.
           MOVE OTK-LINE            TO TK-LINE    ATK-LINE.
     
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
               MOVE OTK-SSNO(SUB)    TO TK-SSNO(SUB) ATK-SSNO(SUB)
               MOVE OTK-MAKERCD(SUB) TO TK-MAKERCD(SUB) ATK-MAKERCD(SUB)
           END-PERFORM.
      
           MOVE OTK-REF-FRTO        TO TK-REF-FRTO ATK-REF-FRTO.
           MOVE OTK-REFBR           TO TK-REFBR    ATK-REFBR.
           MOVE OTK-PURCD           TO TK-PURCD    ATK-PURCD.
           MOVE OTK-SRCD            TO TK-SRCD     ATK-SRCD.
           MOVE OTK-COLLCD          TO TK-COLLCD   ATK-COLLCD.
           MOVE OTK-SERVACCT        TO TK-SERVACCT ATK-SERVACCT.
           MOVE OTK-LOANTYPE        TO TK-LOANTYPE ATK-LOANTYPE.
           MOVE OTK-REPOCD          TO TK-REPOCD   ATK-REPOCD.
           MOVE OTK-PLCD            TO TK-PLCD     ATK-PLCD.
           MOVE OTK-PLTYPE          TO TK-PLTYPE   ATK-PLTYPE.
      
           MOVE OTK-LOANDATE    TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD   TO TK-LOANDATE     ATK-LOANDATE.
           MOVE OTK-PROCEEDS        TO TK-PROCEEDS ATK-PROCEEDS.
           MOVE OTK-LNAMT           TO TK-LNAMT    ATK-LNAMT.
           MOVE OTK-FINCHG          TO TK-FINCHG   ATK-FINCHG.
           MOVE OTK-INTCHG          TO TK-INTCHG   ATK-INTCHG.
           MOVE OTK-SERCHG          TO TK-SERCHG   ATK-SERCHG.
           MOVE OTK-EXTCHG          TO TK-EXTCHG   ATK-EXTCHG.
           MOVE OTK-MAINTFEE        TO TK-MAINTFEE ATK-MAINTFEE.
           MOVE OTK-ORGTERM         TO TK-ORGTERM  ATK-ORGTERM.
           MOVE OTK-INSURED         TO TK-INSURED  ATK-INSURED.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE 0              TO TK-INSUNEARN(SUB)    
                                     ATK-INSUNEARN(SUB)
                                      TK-INSEARNED(SUB)
                                     ATK-INSEARNED(SUB)
           END-PERFORM.
      
           MOVE OTK-DLNO            TO TK-DLNO   ATK-DLNO.
           MOVE OTK-DLRV            TO TK-DLRV   ATK-DLRV.
           MOVE OTK-DLSPRV          TO TK-DLSPRV ATK-DLSPRV.
           MOVE OTK-DLDISC          TO TK-DLDISC ATK-DLDISC.
           MOVE OTK-DLPARV          TO TK-DLPARV ATK-DLPARV.
           MOVE OTK-DLRECOURSE      TO TK-DLRECOURSE  ATK-DLRECOURSE.
           MOVE OTK-DLPARTRCAMT     TO TK-DLPARTRCAMT ATK-DLPARTRCAMT.
           MOVE OTK-PURBAL          TO TK-PURBAL ATK-PURBAL.
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                  MOVE OTK-UNEARN(SUB) TO TK-UNEARN(SUB)
                                         ATK-UNEARN(SUB)
           END-PERFORM.
      
      *CHANGED TK-PRLNNO TO BE THE 8 DIGIT ACCTNO WITHOUT 01 SEQUENCE
           MOVE OTK-PRLNNO            TO TEST-ACCTNO.
           MOVE TEST-NUMBER           TO TK-PRLNNO ATK-PRLNNO.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               MOVE OTK-DLRVPRIN(SUB)   TO TK-DLRVPRIN(SUB)
                                          ATK-DLRVPRIN(SUB)
               MOVE OTK-DLRVCHGS(SUB)   TO TK-DLRVCHGS(SUB)
                                          ATK-DLRVCHGS(SUB)
           END-PERFORM.

           MOVE OTK-DLPARVPAID      TO TK-DLPARVPAID ATK-DLPARVPAID.
           MOVE OTK-DLPARVHELD      TO TK-DLPARVHELD ATK-DLPARVHELD.
           MOVE OTK-USERID          TO TK-USERID     ATK-USERID.

           MOVE OTK-POSTDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TK-POSTDATE   ATK-POSTDATE.

           MOVE OTK-POSTTIME        TO TK-POSTTIME   ATK-POSTTIME.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6  
                  MOVE OTK-EARNED(SUB) TO TK-EARNED(SUB)
                                         ATK-EARNED(SUB)
           END-PERFORM.

           IF (FILE-OPTION-TR = "Y")
              PERFORM WRITE-TRK1-FILE
           ELSE
              ADD 1 TO ATRK-KEY
              ACCEPT ATK-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ATK-CR
              MOVE X"0A"              TO ATK-LF
              PERFORM WRITE-ATRK-FILE.

           IF ( IO-BAD )
              ADD 1               TO TR1534-RECORD-COUNTER-SKIP
              MOVE "TRK"          TO TR1534-CURRENT-FILE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              GO TO IO-ERROR.

       UPGRADE-FILE-TRK-EXIT.           
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R L
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRL-REC  [TRL-REC - DIRECT LOANS]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       UPGRADE-FILE-TRL SECTION.

           IF FILE-OPTION-TR = "Y"
              PERFORM CLEAR-TRL-FILE
           ELSE
              PERFORM CLEAR-ATRL-FILE.

           MOVE TRFILE-MMYY  TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMYY-YY TO DATE-YYMMDD-YY.
           MOVE OTL-DAY      TO DATE-YYMMDD-DD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.

           MOVE OTL-BRNO           TO TL-BRNO     ATL-BRNO.
           MOVE DATE-YYYYMMDD      TO TL-DATE     ATL-DATE.
           MOVE OTL-CLASS          TO TL-CLASS    ATL-CLASS.
           MOVE OTL-NUMBER         TO TL-ACCTNO   ATL-ACCTNO.
           MOVE OTL-LAWCODE        TO TL-LAWCODE  ATL-LAWCODE.
           MOVE OTL-LINE           TO TL-LINE     ATL-LINE.
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
               MOVE OTL-SSNO(SUB)    TO TL-SSNO(SUB)    ATL-SSNO(SUB)
               MOVE OTL-MAKERCD(SUB) TO TL-MAKERCD(SUB) ATL-MAKERCD(SUB)
           END-PERFORM.
      
           MOVE OTL-PURCD          TO TL-PURCD    ATL-PURCD.
           MOVE OTL-SRCD           TO TL-SRCD     ATL-SRCD.
           MOVE OTL-COLLCD         TO TL-COLLCD   ATL-COLLCD.
           MOVE OTL-SERVACCT       TO TL-SERVACCT ATL-SERVACCT.
           MOVE OTL-LOANTYPE       TO TL-LOANTYPE ATL-LOANTYPE.

           MOVE OTL-LOANDATE       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO TL-LOANDATE  ATL-LOANDATE.
           MOVE OTL-INTDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO TL-INTDATE   ATL-INTDATE.
      
           MOVE OTL-PROCEEDS       TO TL-PROCEEDS  ATL-PROCEEDS.
           MOVE OTL-LNAMT          TO TL-LNAMT     ATL-LNAMT.
           MOVE OTL-FINCHG         TO TL-FINCHG    ATL-FINCHG.
           MOVE OTL-INTCHG         TO TL-INTCHG    ATL-INTCHG.
           MOVE OTL-SERCHG         TO TL-SERCHG    ATL-SERCHG.
           MOVE OTL-EXTCHG         TO TL-EXTCHG    ATL-EXTCHG.
           MOVE OTL-MAINTFEE       TO TL-MAINTFEE  ATL-MAINTFEE.
           MOVE OTL-ORGTERM        TO TL-ORGTERM   ATL-ORGTERM.
           MOVE OTL-1STPYAMT       TO TL-1STPYAMT  ATL-1STPYAMT.
           MOVE OTL-NOREGPY        TO TL-NOREGPY   ATL-NOREGPY.
           MOVE OTL-REGPYAMT       TO TL-REGPYAMT  ATL-REGPYAMT.
           MOVE OTL-LASTPYAMT      TO TL-LASTPYAMT ATL-LASTPYAMT.
           MOVE OTL-PROCASH        TO TL-PROCASH   ATL-PROCASH.

           IF OTL-TILLNO = "1"
              MOVE 1  TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "2"
              MOVE 2  TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "3"
              MOVE 3  TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "4"
              MOVE 4  TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "5"
              MOVE 5  TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "6"
              MOVE 6  TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "7"
              MOVE 7  TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "8"
              MOVE 8  TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "9"
              MOVE 9  TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "A"
              MOVE 10 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "B"
              MOVE 11 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "C"
              MOVE 12 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "D"
              MOVE 13 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "E"
              MOVE 14 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "F"
              MOVE 15 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "G"
              MOVE 16 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "H"
              MOVE 17 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "I"
              MOVE 18 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "J"
              MOVE 19 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "K"
              MOVE 20 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "L"
              MOVE 21 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "M"
              MOVE 22 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "N"
              MOVE 23 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "O"
              MOVE 24 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "P"
              MOVE 25 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "Q"
              MOVE 26 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "R"
              MOVE 27 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "S"
              MOVE 28 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "T"
              MOVE 29 TO TL-TILLNO ATL-TILLNO 
           ELSE
           IF OTL-TILLNO = "U"
              MOVE 30 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "V"
              MOVE 31 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "W"
              MOVE 32 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "X"
              MOVE 33 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "Y"
              MOVE 34 TO TL-TILLNO ATL-TILLNO
           ELSE
           IF OTL-TILLNO = "Z"
              MOVE 35 TO TL-TILLNO ATL-TILLNO.
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
                MOVE OTL-FEECODE(SUB) TO TL-FEECODE(SUB)
                                        ATL-FEECODE(SUB)
                MOVE OTL-FEEAMT(SUB)  TO TL-FEEAMT(SUB)
                                        ATL-FEEAMT(SUB)
           END-PERFORM.
           
           MOVE OTL-INSURED        TO TL-INSURED  ATL-INSURED.
           MOVE OTL-INSTYPES       TO TL-INSTYPES  ATL-INSTYPES.
           MOVE OTL-INSELIGIBLE    TO TL-INSELIGIBLE ATL-INSELIGIBLE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE OTL-INSCOMP(SUB)     TO TL-INSCOMP(SUB)
                                           ATL-INSCOMP(SUB)
               MOVE OTL-INSOURS(SUB)     TO TL-INSOURS(SUB)
                                           ATL-INSOURS(SUB)
               MOVE OTL-INSEFF(SUB)      TO TL-INSEFF-DATE(SUB)
                                           ATL-INSEFF-DATE(SUB)
               IF OTL-INSEFF(SUB) > 900
                   MOVE OTL-INSEFF(SUB) TO   DATE-MMDDYY
                   PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                   MOVE DATE-YYYYMMDD TO TL-INSEFF-DATE(SUB)
                                        ATL-INSEFF-DATE(SUB)
               END-IF
               MOVE OTL-INSEXP(SUB)   TO DATE-MMDDYY 
               PERFORM CONVERT-MMDDYY-TO-YYYYMMDD 
               MOVE DATE-YYYYMMDD TO TL-INSEXP-DATE(SUB)
                                    ATL-INSEXP-DATE(SUB)
               MOVE OTL-INSPREM(SUB) TO TL-INSPREM(SUB)
                                       ATL-INSPREM(SUB)
               MOVE OTL-INSCOMM(SUB) TO TL-INSCOMM(SUB)
                                       ATL-INSCOMM(SUB)
           END-PERFORM.

           MOVE OTL-DLNO             TO TL-DLNO  ATL-DLNO.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                 MOVE OTL-EARNINGS(SUB)  TO TL-EARNINGS(SUB)
                                           ATL-EARNINGS(SUB)
                 MOVE OTL-ANTICERN(SUB)  TO TL-ANTICERN(SUB)
                                           ATL-ANTICERN(SUB)
           END-PERFORM.                    
     
      *CHANGED TL-PRLNNO TO BE THE 8 DIGIT ACCTNO WITHOUT 01 SEQUENCE
           MOVE OTL-PRLNNO           TO TEST-ACCTNO.
           MOVE TEST-NUMBER          TO TL-PRLNNO     ATL-PRLNNO.

           MOVE OTL-PRBAL            TO TL-PRBAL      ATL-PRBAL.
           MOVE OTL-PRINTDUE         TO TL-PRINTDUE   ATL-PRINTDUE.
           MOVE OTL-PRFINCHGRB       TO TL-PRFINCHGRB ATL-PRFINCHGRB.
           MOVE OTL-PRINSRB          TO TL-PRINSRB    ATL-PRINSRB.
           MOVE OTL-LPEOD5-FG        TO TL-LPEOD5-FG  ATL-LPEOD5-FG.
           MOVE OTL-STATE-WORKER     TO TL-STATE-WORKER ATL-STATE-WORKER.
           MOVE OTL-RESTRICTD-ACCTNO TO TEST-ACCTNO.
           MOVE TEST-NUMBER          TO TL-RESTRICTD-ACCTNO
                                       ATL-RESTRICTD-ACCTNO.
 
           MOVE OTL-SPRCLASS         TO TL-SPRCLASS ATL-SPRCLASS.
           MOVE OTL-USERID           TO TL-USERID   ATL-USERID.

           MOVE OTL-POSTDATE       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD        TO TL-POSTDATE ATL-POSTDATE.
   
           MOVE OTL-POSTTIME         TO TL-POSTTIME ATL-POSTTIME.

           MOVE OTL-GIFTNET          TO TL-GIFTNET  ATL-GIFTNET.
           IF OTL-APR NOT NUMERIC
              MOVE 0                 TO TL-APR      ATL-APR
           ELSE
              MOVE OTL-APR           TO TL-APR      ATL-APR.
           MOVE OTL-SP-LNCOST-FRMLA  TO TL-SP-LNCOST-FRMLA
                                       ATL-SP-LNCOST-FRMLA.
           MOVE OTL-MIP-FG           TO TL-MIP-FG   ATL-MIP-FG.
           MOVE OTL-INS-TAX-COUNTY-CD  TO TL-INS-TAX-COUNTY-CD
                                         ATL-INS-TAX-COUNTY-CD.
           MOVE OTL-INS-TAX-CITY-CD    TO TL-INS-TAX-CITY-CD
                                         ATL-INS-TAX-CITY-CD.
           MOVE OTL-PRDLDISC-UNEARN    TO TL-PRDLDISC-UNEARN
                                         ATL-PRDLDISC-UNEARN.
           MOVE OTL-POINTS             TO TL-POINTS ATL-POINTS.
           IF OTL-DEBIT-CARD-AMT NOT NUMERIC
              MOVE 0                   TO TL-DEBIT-CARD-AMT
                                         ATL-DEBIT-CARD-AMT
           ELSE
              MOVE OTL-DEBIT-CARD-AMT  TO TL-DEBIT-CARD-AMT
                                         ATL-DEBIT-CARD-AMT.
           MOVE OTL-ACTIONCD2          TO TL-ACTIONCD2 ATL-ACTIONCD2.

           IF (FILE-OPTION-TR = "Y")
              PERFORM WRITE-TRL1-FILE
           ELSE
              ADD 1 TO ATRL-KEY
              ACCEPT ATL-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ATL-CR
              MOVE X"0A"              TO ATL-LF
              PERFORM WRITE-ATRL-FILE.

           IF ( IO-BAD )
              ADD 1               TO TR1534-RECORD-COUNTER-SKIP
              MOVE "TRL"          TO TR1534-CURRENT-FILE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              GO TO IO-ERROR.

       UPGRADE-FILE-TRL-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R P
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRP-REC   [TRP-REC - PAYMENTS]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       UPGRADE-FILE-TRP SECTION.

           IF FILE-OPTION-TR = "Y"
              PERFORM CLEAR-TRP-FILE
           ELSE
              PERFORM CLEAR-ATRP-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TRP-REC IS USED FOR TR-CODE OF "P" "F" OR "G"
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE OTP-BRNO     TO TP-BRNO    ATP-BRNO.

           MOVE TRFILE-MMYY  TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMYY-YY TO DATE-YYMMDD-YY.
           MOVE OTP-DAY      TO DATE-YYMMDD-DD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD    TO TP-DATE        ATP-DATE.

      *    MOVE OTP-CODE         TO TP-CODE        ATP-CODE

      * OTP-CODE 'F' ARE PAYMENTS WITH POOLS
      * OTP-CODE 'G' ARE SP SET POOL TRANSACTIONS
      * "F" AND "G" RECORDS ARE FIRST IN THE OLD FILE
      * HOWEVER, THEY WERE WRITTEN AFTER THE OLD "P" RECORDS AND ADDED 1 TO
      * THE LINE, SO SUBTRACT 1 FROM THE LINE AND WRITE 
      * TP-DAY, TP-BRNO, TP-CODE
      * SKIP THESE TRANSACTIONS DUE TO NOT HAVING TP-EARNINGS IN THEM. HAVE TO
      * WRITE TO ACTUAL "P" RECORD AND READ THE WK-FILE TO GET THE POOLID
      * THE "G" RECORD IS AN ABSOLUTE COPY OF THE "P", "SP" SO I CAN SKIP IT
      * "F" AND "G" RECORDS NEED TO ONLY WRITE A WORK RECORD TO SAVE THE
      * POOLID
           IF OTP-CODE = "F" OR "G"
              MOVE TP-BRNO     TO  WORK-BRNO
              MOVE TP-DATE     TO  WORK-DATE
              MOVE OTPF-CLASS  TO  WORK-CLASS
              MOVE OTPF-NUMBER TO  WORK-ACCTNO
              SUBTRACT 1 FROM OTPF-LINE GIVING WORK-LINE
              MOVE OTPF-POOLID TO WORK-POOLID
              PERFORM WRITE-WK-FILE
              GO TO UPGRADE-FILE-TRP-EXIT.

           MOVE 0                TO TP-POOLID      ATP-POOLID.
           MOVE OTP-CLASS        TO TP-CLASS       ATP-CLASS.
           MOVE OTP-NUMBER       TO TP-ACCTNO      ATP-ACCTNO.
           MOVE OTP-LINE         TO TP-LINE        ATP-LINE.


           MOVE OTP-LAWCODE      TO TP-LAWCODE     ATP-LAWCODE.
           MOVE OTP-JDCD         TO TP-JDCD        ATP-JDCD.
           MOVE OTP-PLCD         TO TP-PLCD        ATP-PLCD.
           MOVE OTP-SSNO         TO TP-SSNO        ATP-SSNO.
           MOVE OTP-MAKER        TO TP-MAKER       ATP-MAKER.
           MOVE OTP-IBPC         TO TP-IBPC        ATP-IBPC.
           MOVE OTP-TRCD         TO TP-TRCD        ATP-TRCD.
           MOVE OTP-REFNO        TO TP-REFNO       ATP-REFNO.
           IF OTP-TRCD = "TP" OR "TS"
              MOVE OTP-REFNO     TO TEST-REFNO-TRANSFER
              MOVE TEST-FROMCL   TO TP-FROMCL      ATP-FROMCL
              MOVE TEST-TOCL     TO TP-TOCL        ATP-TOCL.

           MOVE OTP-TRCD         TO TEST-TRCD. 

           IF ADDON-TRCD 
              MOVE OTP-ADDON-TEXT   TO TP-ADDON-TXT    ATP-ADDON-TXT
              MOVE OTP-ADDON-TYPE   TO TP-ADDON-TYP    ATP-ADDON-TYP
              MOVE 0                TO TP-DLRVNUM      ATP-DLRVNUM
                                       TP-P4P5-DLRVNUM ATP-P4P5-DLRVNUM
              MOVE SPACES           TO TP-P4P5-TRANCD  ATP-P4P5-TRANCD
           ELSE
           IF RESERVE-TRCD
              MOVE OTP-DLRVNO       TO TP-DLRVNUM      ATP-DLRVNUM
              MOVE SPACES           TO TP-ADDON-TXT    ATP-ADDON-TXT
                                       TP-ADDON-TYP    ATP-ADDON-TYP
                                       TP-P4P5-TRANCD  ATP-P4P5-TRANCD
              MOVE 0                TO TP-P4P5-DLRVNUM ATP-P4P5-DLRVNUM
           ELSE
           IF RESERVE-P4P5-TRCD
              MOVE OTP-P4P5-DLRVNO  TO TP-P4P5-DLRVNUM ATP-P4P5-DLRVNUM
              MOVE OTP-P4P5-TRCD    TO TP-P4P5-TRANCD  ATP-P4P5-TRANCD
              MOVE  0               TO TP-DLRVNUM      ATP-DLRVNUM
              MOVE SPACES           TO TP-ADDON-TXT    ATP-ADDON-TXT
                                       TP-ADDON-TYP    ATP-ADDON-TYP
           ELSE
              MOVE SPACES           TO TP-ADDON-TXT    ATP-ADDON-TXT
                                       TP-ADDON-TYP    ATP-ADDON-TYP
                                       TP-P4P5-TRANCD  ATP-P4P5-TRANCD
              MOVE 0                TO TP-P4P5-DLRVNUM ATP-P4P5-DLRVNUM
                                       TP-DLRVNUM      ATP-DLRVNUM.

           MOVE OTP-PAYDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TP-PAYDATE      ATP-PAYDATE.

           MOVE OTP-DUEDATE         TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TP-DUEDATE      ATP-DUEDATE.
      
           MOVE OTP-TRAMT           TO TP-TRAMT        ATP-TRAMT.
           MOVE OTP-INTDUE          TO TP-INTDUE       ATP-INTDUE.
           MOVE OTP-APOTH           TO TP-APOTH        ATP-APOTH.
           MOVE OTP-APOT2           TO TP-APOT2        ATP-APOT2.
           MOVE OTP-APINT           TO TP-APINT        ATP-APINT.
           MOVE OTP-APLC            TO TP-APLC         ATP-APLC.
           MOVE OTP-APCUR           TO TP-APCUR        ATP-APCUR.
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE OTP-GLNO(SUB)   TO TP-GLNO(SUB)  ATP-GLNO(SUB)
              MOVE OTP-GLAMT(SUB)  TO TP-GLAMT(SUB) ATP-GLAMT(SUB)
           END-PERFORM.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                 MOVE OTP-EARNINGS(SUB) TO TP-EARNINGS(SUB)
                                          ATP-EARNINGS(SUB)
                 MOVE OTP-WORKAMT(SUB)  TO TP-WORKAMT(SUB)
                                          ATP-WORKAMT(SUB)
           END-PERFORM.

           MOVE OTP-OTHBAL         TO TP-OTHBAL  ATP-OTHBAL.
           MOVE OTP-OT2BAL         TO TP-OT2BAL  ATP-OT2BAL.
           MOVE OTP-INTBAL         TO TP-INTBAL  ATP-INTBAL.
           MOVE OTP-LCBAL          TO TP-LCBAL   ATP-LCBAL.
           MOVE OTP-CURBAL         TO TP-CURBAL  ATP-CURBAL.

           MOVE OTP-PDTHRU         TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO TP-PDTH-DATE ATP-PDTH-DATE.
      
           MOVE OTP-DLNO           TO TP-DLNO      ATP-DLNO.
           MOVE OTP-DLPROC         TO TP-DLPROC    ATP-DLPROC.

           MOVE OTP-SERVACCT       TO TP-SERVACCT ATP-SERVACCT.
           MOVE OTP-SALEFIN        TO TP-SALEFIN  ATP-SALEFIN.
           MOVE OTP-INSURED        TO TP-INSURED  ATP-INSURED.
           MOVE OTP-INSOURS        TO TP-INSOURS  ATP-INSOURS.
           MOVE OTP-LOANTYPE       TO TP-LOANTYPE ATP-LOANTYPE.

           IF OTP-TILLNO = "1"
              MOVE 1  TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "2"
              MOVE 2  TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "3"
              MOVE 3  TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "4"
              MOVE 4  TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "5"
              MOVE 5  TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "6"
              MOVE 6  TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "7"
              MOVE 7  TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "8"
              MOVE 8  TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "9"
              MOVE 9  TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "A"
              MOVE 10 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "B"
              MOVE 11 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "C"
              MOVE 12 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "D"
              MOVE 13 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "E"
              MOVE 14 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "F"
              MOVE 15 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "G"
              MOVE 16 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "H"
              MOVE 17 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "I"
              MOVE 18 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "J"
              MOVE 19 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "K"
              MOVE 20 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "L"
              MOVE 21 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "M"
              MOVE 22 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "N"
              MOVE 23 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "O"
              MOVE 24 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "P"
              MOVE 25 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "Q"
              MOVE 26 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "R"
              MOVE 27 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "S"
              MOVE 28 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "T"
              MOVE 29 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "U"
              MOVE 30 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "V"
              MOVE 31 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "W"
              MOVE 32 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "X"
              MOVE 33 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "Y"
              MOVE 34 TO TP-TILLNO ATP-TILLNO
           ELSE
           IF OTP-TILLNO = "Z"
              MOVE 35 TO TP-TILLNO ATP-TILLNO.
      
           MOVE OTP-LCPD           TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO TP-LCPDTH-DATE  ATP-LCPDTH-DATE.

           MOVE OTP-POSTING-BR      TO TP-POSTING-BR  ATP-POSTING-BR.
           MOVE OTP-OTHBR-FG        TO TP-OTHBR-FG    ATP-OTHBR-FG.
           MOVE OTP-REV             TO TP-REV         ATP-REV.
           MOVE OTP-REVTRCD         TO TP-REVTRCD     ATP-REVTRCD.
           MOVE OTP-REVSEQNO        TO TP-REVSEQNO    ATP-REVSEQNO.
           MOVE OTP-STATUSFG        TO TP-STATUSFG    ATP-STATUSFG.
           MOVE OTP-KEEPAPINT       TO TP-KEEPAPINT   ATP-KEEPAPINT.
           MOVE OTP-INTPAID         TO TP-INTPAID     ATP-INTPAID.
           MOVE OTP-PRIOR-POOLID    TO TP-PRIOR-POOLID ATP-PRIOR-POOLID.
           MOVE OTP-INSTYPES        TO TP-INSTYPES    ATP-INSTYPES.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              MOVE ZEROS            TO TP-INSEARN(SUB)
                                      ATP-INSEARN(SUB)
                                       TP-INSWORKAMT(SUB)
                                      ATP-INSWORKAMT(SUB)
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3  
              MOVE OTP-DLRVCHGS-ADJ(SUB) TO TP-DLRVCHGS-ADJ(SUB)
                                           ATP-DLRVCHGS-ADJ(SUB)
           END-PERFORM. 

           MOVE OTP-DLPARVHELD-ADJ  TO TP-DLPARVHELD-ADJ
                                      ATP-DLPARVHELD-ADJ.
           MOVE OTP-DLPARVPAID-ADJ  TO TP-DLPARVPAID-ADJ
                                      ATP-DLPARVPAID-ADJ.
           MOVE OTP-NEWSTAT-RATE    TO TP-NEWSTAT-RATE
                                      ATP-NEWSTAT-RATE.
           MOVE OTP-POCD            TO TP-POCD   ATP-POCD.

           MOVE OTP-PODATE         TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO TP-PODATE  ATP-PODATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE OTP-DL-RVCHGS-CODE(SUB) TO TP-DL-RVCHGS-CODE(SUB)
                                             ATP-DL-RVCHGS-CODE(SUB)
           END-PERFORM.

           MOVE OTP-DL-PARVHELD-CODE  TO TP-DL-PARVHELD-CODE
                                        ATP-DL-PARVHELD-CODE.
           MOVE OTP-DL-PARVPAID-CODE  TO TP-DL-PARVPAID-CODE
                                        ATP-DL-PARVPAID-CODE.
           MOVE OTP-TOTNODEF          TO TP-TOTNODEF ATP-TOTNODEF.
           MOVE OTP-LNAMT             TO TP-LNAMT    ATP-LNAMT.

           MOVE OTP-ORIG-1STPAY       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           IF DATE-YYYYMMDD-MM = 2 AND DATE-YYYYMMDD-DD = 30
              MOVE 28 TO DATE-YYYYMMDD-DD.
           MOVE DATE-YYYYMMDD         TO TP-ORIG-1STPYDATE
                                        ATP-ORIG-1STPYDATE.
              
           MOVE OTP-OLD1ST-PYDATE     TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD         TO TP-OLD1ST-PYDATE
                                        ATP-OLD1ST-PYDATE.

           MOVE OTP-1STPYDATE         TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD         TO TP-1STPYDATE ATP-1STPYDATE.

           MOVE OTP-ORGTERM         TO TP-ORGTERM    ATP-ORGTERM.
           MOVE OTP-INSCOMM         TO TP-INSCOMM    ATP-INSCOMM.
           MOVE OTP-MANUAL-RB       TO TP-MANUAL-RB  ATP-MANUAL-RB.
           MOVE OTP-USERID          TO TP-USERID     ATP-USERID.
           MOVE OTP-POSTTIME        TO TP-POSTTIME   ATP-POSTTIME.
           MOVE OTP-PLTYPE          TO TP-PLTYPE     ATP-PLTYPE.
           MOVE OTP-PL-OTH-AR       TO TP-PL-OTH-AR  ATP-PL-OTH-AR.
           MOVE OTP-LP-DLNO         TO TP-LP-DLNO    ATP-LP-DLNO.
           MOVE OTP-REPOCD          TO TP-REPOCD     ATP-REPOCD.

           MOVE OTP-SPRCLASS        TO TP-SPRCLASS   ATP-SPRCLASS.
           MOVE OTP-MAX-DEFER-FG    TO TP-MAX-DEFER-FG ATP-MAX-DEFER-FG.

           MOVE OTP-POSTDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TP-POSTDATE   ATP-POSTDATE.

           MOVE OTP-UNITPER-CD      TO TP-UNITPER-CD  ATP-UNITPER-CD.
           MOVE OTP-UNITPER-FREQ    TO TP-UNITPER-FREQ ATP-UNITPER-FREQ.
           MOVE OTP-SP-DEFFRMLA     TO TP-SP-DEFFRMLA ATP-SP-DEFFRMLA.
           MOVE OTP-COLLECTOR       TO TP-COLLECTOR   ATP-COLLECTOR.
           MOVE OTP-PENALTY-INT     TO TP-PENALTY-INT ATP-PENALTY-INT.
           MOVE OTP-INSCOMP         TO TP-INSCOMP     ATP-INSCOMP.

           MOVE OTP-PROMDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TP-PROMDATE   ATP-PROMDATE.

            MOVE OTP-PROMAMT        TO TP-PROMAMT    ATP-PROMAMT.
            MOVE OTP-PROMID         TO TP-PROMID     ATP-PROMID.
            MOVE OTP-INS-TAX-RB     TO TP-INS-TAX-RB ATP-INS-TAX-RB.
           IF OTP-PROMDAYS-PASTDUE NOT NUMERIC
              MOVE 0                    TO TP-PROMDAYS-PASTDUE
                                          ATP-PROMDAYS-PASTDUE
           ELSE
              MOVE OTP-PROMDAYS-PASTDUE TO TP-PROMDAYS-PASTDUE
                                          ATP-PROMDAYS-PASTDUE.
            MOVE OTP-MIP-FG         TO TP-MIP-FG     ATP-MIP-FG.
            MOVE OTP-MIP-CLPAID     TO TP-MIP-CLPAID ATP-MIP-CLPAID..
            MOVE OTP-MIP-CLCOMM     TO TP-MIP-CLCOMM ATP-MIP-CLCOMM..
            MOVE OTP-MIP-AHPAID     TO TP-MIP-AHPAID ATP-MIP-AHPAID.
            MOVE OTP-MIP-AHCOMM     TO TP-MIP-AHCOMM ATP-MIP-AHCOMM.

           MOVE OTP-MIP-CLPDTH      TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TP-MIP-CLPDTH-DATE
                                      ATP-MIP-CLPDTH-DATE.

           MOVE OTP-MIP-AHPDTH      TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TP-MIP-AHPDTH-DATE
                                      ATP-MIP-AHPDTH-DATE.

           MOVE OTP-INS-TAX-COUNTY-CD   TO TP-INS-TAX-COUNTY-CD
                                          ATP-INS-TAX-COUNTY-CD.
           MOVE OTP-INS-TAX-CITY-CD     TO TP-INS-TAX-CITY-CD
                                          ATP-INS-TAX-CITY-CD.

           MOVE OTP-APPLIED-TO-ESCROW  TO TP-APPLIED-TO-ESCROW
                                         ATP-APPLIED-TO-ESCROW.
           MOVE OTP-ESCROW-AMT         TO TP-ESCROW-AMT
                                         ATP-ESCROW-AMT.
           MOVE OTP-ESCROW-PREPAID     TO TP-ESCROW-PREPAID
                                         ATP-ESCROW-PREPAID.
           MOVE OTP-ESCROW-DISBURSED   TO TP-ESCROW-DISBURSED
                                         ATP-ESCROW-DISBURSED.

           MOVE OTP-LP-SEQNO      TO TP-LP-SEQNO ATP-LP-SEQNO.
           MOVE OTP-PRIOR-DF-CODE TO TP-PRIOR-DF-CODE ATP-PRIOR-DF-CODE.

           IF OTP-PRIOR-DF-POSTDATE NOT NUMERIC OR 
               OTP-PRIOR-DF-POSTDATE = 202020
              MOVE 0 TO TP-PRIOR-DF-POSTDATE ATP-PRIOR-DF-POSTDATE
           ELSE
              MOVE OTP-PRIOR-DF-POSTDATE TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD         TO TP-PRIOR-DF-POSTDATE
                                           ATP-PRIOR-DF-POSTDATE.

           MOVE OTP-CANCEL-REASON TO TP-CANCEL-REASON ATP-CANCEL-REASON.
           MOVE OTP-ACTIONCD2     TO TP-ACTIONCD2     ATP-ACTIONCD2.
           IF OTP-LCDUE NOT NUMERIC OR
               OTP-LCDUE = 20202.02 
              MOVE 0              TO TP-LCDUE  ATP-LCDUE
           ELSE
              MOVE OTP-LCDUE      TO TP-LCDUE  ATP-LCDUE.

           IF OTP-REPAY-TRANS-ID NOT NUMERIC OR
               (OTP-REPAY-TRANS-ID = 2020202020 OR 0202020202)
              MOVE 0 TO TP-REPAY-TRANS-ID  ATP-REPAY-TRANS-ID
           ELSE
              MOVE OTP-REPAY-TRANS-ID TO TP-REPAY-TRANS-ID 
                                        ATP-REPAY-TRANS-ID.

           IF OTP-CLOSFEE-REB-AMT NOT NUMERIC OR
               (OTP-CLOSFEE-REB-AMT = 20202.02 OR 202020.20)
              MOVE 0              TO TP-CLOSFEE-REB-AMT 
                                    ATP-CLOSFEE-REB-AMT
           ELSE
              MOVE OTP-CLOSFEE-REB-AMT TO TP-CLOSFEE-REB-AMT 
                                         ATP-CLOSFEE-REB-AMT.

      * WORK FILE WAS WRITTEN UP ABOVE FOR "F" AND "G" RECORDS AND THEN THEY
      * EXIT.
      * READ WK-FILE FOR ALL "P" RECORDS, NEED TO SEE IF THERE WAS AN "F"
      * TO GET JUST THE POOLID FROM, THE "P" RECORD DOESNT HAVE IT. 
              MOVE TP-BRNO   TO  WORK-BRNO
              MOVE TP-DATE   TO  WORK-DATE
              MOVE TP-CLASS  TO  WORK-CLASS
              MOVE TP-ACCTNO TO  WORK-ACCTNO
              MOVE TP-LINE   TO  WORK-LINE
              PERFORM READ-WK-FILE
              IF IO-FG = 0
                 MOVE WORK-POOLID TO TP-POOLID ATP-POOLID.
                
           IF (FILE-OPTION-TR = "Y")
              PERFORM WRITE-TRP1-FILE
           ELSE
              ADD 1 TO ATRP-KEY
              ACCEPT ATP-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ATP-CR
              MOVE X"0A"              TO ATP-LF
              PERFORM WRITE-ATRP-FILE.

           IF ( IO-BAD )
              ADD 1               TO TR1534-RECORD-COUNTER-SKIP
              MOVE "TRP"          TO TR1534-CURRENT-FILE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              GO TO IO-ERROR.

       UPGRADE-FILE-TRP-EXIT.
           EXIT.

      ****************************************
       NEED-TO-REWRITE-TRP SECTION.
      ****************************************
           IF (FILE-OPTION-TR = "Y")
              PERFORM REWRITE-TRP1-FILE
           ELSE
              ADD 1 TO ATRP-KEY
              ACCEPT ATP-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ATP-CR
              MOVE X"0A"              TO ATP-LF
              PERFORM WRITE-ATRP-FILE.

           IF ( IO-BAD )
              ADD 1               TO TR1534-RECORD-COUNTER-SKIP
              MOVE "TRP"          TO TR1534-CURRENT-FILE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              GO TO IO-ERROR.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R S
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRS-REC   [TRS-REC - SALES FINANCE NEW INPUT]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       UPGRADE-FILE-TRS SECTION.

           IF FILE-OPTION-TR = "Y"
              PERFORM CLEAR-TRS-FILE
           ELSE
              PERFORM CLEAR-ATRS-FILE.

           MOVE OTS-BRNO            TO TS-BRNO  ATS-BRNO.

           MOVE TRFILE-MMYY  TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMYY-YY TO DATE-YYMMDD-YY.
           MOVE OTB-DAY      TO DATE-YYMMDD-DD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TS-DATE     ATS-DATE.
           MOVE OTS-CLASS           TO TS-CLASS    ATS-CLASS.
           MOVE OTS-NUMBER          TO TS-ACCTNO   ATS-ACCTNO.
           MOVE OTS-LAWCODE         TO TS-LAWCODE  ATS-LAWCODE.
           MOVE OTS-LINE            TO TS-LINE     ATS-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
               MOVE OTS-SSNO(SUB)    TO TS-SSNO(SUB)  ATS-SSNO(SUB)
               MOVE OTS-MAKERCD(SUB) TO TS-MAKERCD(SUB) ATS-MAKERCD(SUB)
           END-PERFORM.

           MOVE OTS-PURCD           TO TS-PURCD     ATS-PURCD.
           MOVE OTS-SRCD            TO TS-SRCD      ATS-SRCD.
           MOVE OTS-COLLCD          TO TS-COLLCD    ATS-COLLCD.
           MOVE OTS-SERVACCT        TO TS-SERVACCT  ATS-SERVACCT.
           MOVE OTS-LOANTYPE        TO TS-LOANTYPE  ATS-LOANTYPE.
      
           MOVE OTS-LOANDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TS-LOANDATE        ATS-LOANDATE.
           MOVE OTS-INTDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TS-INTDATE         ATS-INTDATE.
           MOVE OTS-PROCEEDS        TO TS-PROCEEDS  ATS-PROCEEDS.
           MOVE OTS-LNAMT           TO TS-LNAMT     ATS-LNAMT.
           MOVE OTS-FINCHG          TO TS-FINCHG    ATS-FINCHG.
           MOVE OTS-INTCHG          TO TS-INTCHG    ATS-INTCHG.
           MOVE OTS-SERCHG          TO TS-SERCHG    ATS-SERCHG.
           MOVE OTS-EXTCHG          TO TS-EXTCHG    ATS-EXTCHG.
           MOVE OTS-MAINTFEE        TO TS-MAINTFEE  ATS-MAINTFEE.
           MOVE OTS-ORGTERM         TO TS-ORGTERM   ATS-ORGTERM.
           MOVE OTS-1STPYAMT        TO TS-1STPYAMT  ATS-1STPYAMT.
           MOVE OTS-NOREGPY         TO TS-NOREGPY   ATS-NOREGPY.
           MOVE OTS-REGPYAMT        TO TS-REGPYAMT  ATS-REGPYAMT.
           MOVE OTS-LASTPYAMT       TO TS-LASTPYAMT ATS-LASTPYAMT.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
                MOVE OTS-FEECODE(SUB)    TO TS-FEECODE(SUB)
                                           ATS-FEECODE(SUB)
                MOVE OTS-FEEAMT(SUB)     TO TS-FEEAMT(SUB)    
                                           ATS-FEEAMT(SUB)
                MOVE OTS-FEEOURS(SUB)    TO TS-FEEOURS(SUB)
                                           ATS-FEEOURS(SUB)
           END-PERFORM.                                       
       
           MOVE OTS-INSURED         TO TS-INSURED      ATS-INSURED.
           MOVE OTS-INSTYPES        TO TS-INSTYPES     ATS-INSTYPES.
           MOVE OTS-INSELIGIBLE     TO TS-INSELIGIBLE  ATS-INSELIGIBLE.
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                  MOVE OTS-INSCOMP(SUB) TO TS-INSCOMP(SUB)
                                          ATS-INSCOMP(SUB)
                  MOVE OTS-INSOURS(SUB) TO TS-INSOURS(SUB)
                                          ATS-INSOURS(SUB)
                  MOVE OTS-INSEFF(SUB)  TO TS-INSEFF-DATE(SUB)
                                          ATS-INSEFF-DATE(SUB)
                  IF OTS-INSEFF(SUB) > 900
                     MOVE OTS-INSEFF(SUB)   TO DATE-MMDDYY
                     PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                     MOVE DATE-YYYYMMDD TO TS-INSEFF-DATE(SUB)
                                          ATS-INSEFF-DATE(SUB)
                  END-IF
                  MOVE OTS-INSEXP(SUB)  TO DATE-MMDDYY    
                  PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                  MOVE DATE-YYYYMMDD TO TS-INSEXP-DATE(SUB)
                                       ATS-INSEXP-DATE(SUB)
                  MOVE OTS-INSPREM(SUB) TO TS-INSPREM(SUB)
                                          ATS-INSPREM(SUB)
                  MOVE OTS-INSCOMM(SUB) TO TS-INSCOMM(SUB)
                                          ATS-INSCOMM(SUB)
           END-PERFORM.

           MOVE OTS-DLCOMM-PP-CMSI  TO TS-DLCOMM-PP-CMSI
                                      ATS-DLCOMM-PP-CMSI.
           MOVE OTS-DLCOMM-O3-CMSI  TO TS-DLCOMM-O3-CMSI
                                      ATS-DLCOMM-O3-CMSI.
           MOVE OTS-INS-TAX-COUNTY-CD   TO TS-INS-TAX-COUNTY-CD
                                          ATS-INS-TAX-COUNTY-CD.
           MOVE OTS-INS-TAX-CITY-CD TO TS-INS-TAX-CITY-CD
                                      ATS-INS-TAX-CITY-CD.

           IF OTS-TILLNO = "1"
              MOVE 1  TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "2"
              MOVE 2  TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "3"
              MOVE 3  TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "4"
              MOVE 4  TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "5"
              MOVE 5  TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "6"
              MOVE 6  TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "7"
              MOVE 7  TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "8"
              MOVE 8  TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "9"
              MOVE 9  TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "A"
              MOVE 10 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "B"
              MOVE 11 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "C"
              MOVE 12 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "D"
              MOVE 13 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "E"
              MOVE 14 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "F"
              MOVE 15 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "G"
              MOVE 16 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "H"
              MOVE 17 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "I"
              MOVE 18 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "J"
              MOVE 19 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "K"
              MOVE 20 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "L"
              MOVE 21 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "M"
              MOVE 22 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "N"
              MOVE 23 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "O"
              MOVE 24 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "P"
              MOVE 25 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "Q"
              MOVE 26 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "R"
              MOVE 27 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "S"
              MOVE 28 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "T"
              MOVE 29 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "U"
              MOVE 30 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "V"
              MOVE 31 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "W"
              MOVE 32 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "X"
              MOVE 33 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "Y"
              MOVE 34 TO TS-TILLNO ATS-TILLNO
           ELSE
           IF OTS-TILLNO = "Z"
              MOVE 35 TO TS-TILLNO ATS-TILLNO.

           MOVE OTS-UNWIND-REASON-CODE TO TS-UNWIND-REASON-CODE
                                         ATS-UNWIND-REASON-CODE.
           MOVE OTS-DLNO            TO TS-DLNO       ATS-DLNO.
           MOVE OTS-DLRECOURSE      TO TS-DLRECOURSE ATS-DLRECOURSE.
           MOVE OTS-DLPARTRCAMT     TO TS-DLPARTRCAMT ATS-DLPARTRCAMT.
           MOVE OTS-PURBAL          TO TS-PURBAL     ATS-PURBAL.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               MOVE OTS-DLRVPRIN(SUB) TO TS-DLRVPRIN(SUB)
                                        ATS-DLRVPRIN(SUB)
               MOVE OTS-DLRVCHGS(SUB) TO TS-DLRVCHGS(SUB)
                                        ATS-DLRVCHGS(SUB)
               MOVE OTS-DL-RVCHGS-CODE(SUB) TO TS-DL-RVCHGS-CODE(SUB)
                                              ATS-DL-RVCHGS-CODE(SUB)
           END-PERFORM.

           MOVE OTS-DL-PARVHELD-CODE  TO TS-DL-PARVHELD-CODE
                                        ATS-DL-PARVHELD-CODE.
           MOVE OTS-DL-PARVPAID-CODE  TO TS-DL-PARVPAID-CODE
                                        ATS-DL-PARVPAID-CODE.

           MOVE OTS-DLPARVHELD      TO TS-DLPARVHELD ATS-DLPARVHELD.
           MOVE OTS-DLPARVPAID      TO TS-DLPARVPAID ATS-DLPARVPAID.
           MOVE OTS-DLDISC          TO TS-DLDISC     ATS-DLDISC.
           MOVE OTS-ANTICTERM       TO TS-ANTICTERM  ATS-ANTICTERM.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE OTS-ANTICERN(SUB) TO TS-ANTICERN(SUB)
                                        ATS-ANTICERN(SUB)
               MOVE OTS-EARNINGS(SUB) TO TS-EARNINGS(SUB)
                                        ATS-EARNINGS(SUB)
           END-PERFORM.
      
      *CHANGED TS-PRLNNO TO BE THE 8 DIGIT ACCTNO WITHOUT 01 SEQUENCE
           MOVE OTS-PRLNNO            TO TEST-ACCTNO.
           MOVE TEST-NUMBER           TO TS-PRLNNO ATS-PRLNNO.       

           MOVE OTS-INSPREM-DLADJ   TO TS-INSPREM-DLADJ
                                      ATS-INSPREM-DLADJ.
           MOVE OTS-PRDLDISC-UNEARN TO TS-PRDLDISC-UNEARN
                                      ATS-PRDLDISC-UNEARN.
           MOVE OTS-SP-LNCOST-FRMLA TO TS-SP-LNCOST-FRMLA
                                      ATS-SP-LNCOST-FRMLA.
           MOVE OTS-DLPAID          TO TS-DLPAID   ATS-DLPAID.
           MOVE OTS-MIP-FG          TO TS-MIP-FG   ATS-MIP-FG.
           MOVE OTS-SPRCLASS        TO TS-SPRCLASS ATS-SPRCLASS.
           MOVE OTS-O3INS-DLPROC    TO TS-O3INS-DLPROC ATS-O3INS-DLPROC.
           MOVE OTS-USERID          TO TS-USERID   ATS-USERID.
      
           MOVE OTS-POSTDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD   TO TS-POSTDATE ATS-POSTDATE.

           MOVE OTS-POSTTIME        TO TS-POSTTIME ATS-POSTTIME.
           MOVE 0                   TO TS-TAX-AMT  ATS-TAX-AMT.
           MOVE OTS-ACTIONCD2       TO TS-ACTIONCD2 ATS-ACTIONCD2.

           IF (FILE-OPTION-TR = "Y")
              PERFORM WRITE-TRS1-FILE
           ELSE
              ADD 1 TO ATRS-KEY
              ACCEPT ATS-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ATS-CR
              MOVE X"0A"              TO ATS-LF
              PERFORM WRITE-ATRS-FILE.

           IF ( IO-BAD )
              ADD 1               TO TR1534-RECORD-COUNTER-SKIP
              MOVE "TRS"          TO TR1534-CURRENT-FILE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              GO TO IO-ERROR.
       UPGRADE-FILE-TRS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R W
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRW-REC   [ TRW-REC - CASH BALANCING ]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       UPGRADE-FILE-TRW SECTION.

           IF FILE-OPTION-TR = "Y"
              PERFORM CLEAR-TRW-FILE
           ELSE
              PERFORM CLEAR-ATRW-FILE.

           MOVE OTW-BRNO            TO TW-BRNO  ATW-BRNO.

           MOVE TRFILE-MMYY  TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMYY-YY TO DATE-YYMMDD-YY.
           MOVE OTW-DAY      TO DATE-YYMMDD-DD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.

           MOVE DATE-YYYYMMDD TO TW-DATE        ATW-DATE.
      
           IF OTW-TILLNO = "1"
              MOVE 1  TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "2"
              MOVE 2  TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "3"
              MOVE 3  TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "4"
              MOVE 4  TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "5"
              MOVE 5  TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "6"
              MOVE 6  TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "7"
              MOVE 7  TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "8"
              MOVE 8  TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "9"
              MOVE 9  TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "A"
              MOVE 10 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "B"
              MOVE 11 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "C"
              MOVE 12 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "D"
              MOVE 13 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "E"
              MOVE 14 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "F"
              MOVE 15 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "G"
              MOVE 16 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "H"
              MOVE 17 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "I"
              MOVE 18 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "J"
              MOVE 19 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "K"
              MOVE 20 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "L"
              MOVE 21 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "M"
              MOVE 22 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "N"
              MOVE 23 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "O"
              MOVE 24 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "P"
              MOVE 25 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "Q"
              MOVE 26 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "R"
              MOVE 27 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "S"
              MOVE 28 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "T"
              MOVE 29 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "U"
              MOVE 30 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "V"
              MOVE 31 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "W"
              MOVE 32 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "X"
              MOVE 33 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "Y"
              MOVE 34 TO TW-TILLNO ATW-TILLNO
           ELSE
           IF OTW-TILLNO = "Z"
              MOVE 35 TO TW-TILLNO ATW-TILLNO.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
               MOVE OTW-POSTNAME(SUB) TO TW-POSTNAME(SUB)
                                        ATW-POSTNAME(SUB)
               MOVE OTW-TIME(SUB)     TO TW-TIME(SUB)
                                        ATW-TIME(SUB)
               MOVE OTW-AMOUNT(SUB)   TO TW-AMOUNT(SUB)
                                        ATW-AMOUNT(SUB)
               MOVE OTW-OVERSHORT(SUB) TO TW-OVERSHORT(SUB)
                                         ATW-OVERSHORT(SUB)
           END-PERFORM.

           IF (FILE-OPTION-TR = "Y")
              PERFORM WRITE-TRW1-FILE
           ELSE
              ADD 1 TO ATRW-KEY
              ACCEPT ATW-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ATW-CR
              MOVE X"0A"              TO ATW-LF
              PERFORM WRITE-ATRW-FILE.

           IF ( IO-BAD )
              ADD 1               TO TR1534-RECORD-COUNTER-SKIP
              MOVE "TRW"          TO TR1534-CURRENT-FILE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              GO TO IO-ERROR.

       UPGRADE-FILE-TRW-EXIT.          
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R X
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRX-REC   [ TRX-REC - CLASS TRANSFER ]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       UPGRADE-FILE-TRX SECTION.

           IF FILE-OPTION-TR = "Y"
              PERFORM CLEAR-TRX-FILE
           ELSE
              PERFORM CLEAR-ATRX-FILE.

           MOVE OTX-BRNO            TO TX-BRNO  ATX-BRNO.

           MOVE TRFILE-MMYY  TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMYY-YY TO DATE-YYMMDD-YY.
           MOVE OTX-DAY      TO DATE-YYMMDD-DD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.

           MOVE DATE-YYYYMMDD       TO TX-DATE    ATX-DATE.
           MOVE OTX-CLASS           TO TX-CLASS   ATX-CLASS.
           MOVE OTX-NUMBER          TO TX-ACCTNO  ATX-ACCTNO.
           MOVE OTX-LAWCODE         TO TX-LAWCODE ATX-LAWCODE.
           MOVE OTX-LINE            TO TX-LINE    ATX-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
               MOVE OTX-SSNO(SUB)    TO TX-SSNO(SUB)    ATX-SSNO(SUB)
               MOVE OTX-MAKERCD(SUB) TO TX-MAKERCD(SUB) ATX-MAKERCD(SUB)
           END-PERFORM.

           MOVE OTX-PURCD           TO TX-PURCD    ATX-PURCD.
           MOVE OTX-SRCD            TO TX-SRCD     ATX-SRCD.
           MOVE OTX-COLLCD          TO TX-COLLCD   ATX-COLLCD.
           MOVE OTX-SERVACCT        TO TX-SERVACCT ATX-SERVACCT.
           MOVE OTX-LOANTYPE        TO TX-LOANTYPE ATX-LOANTYPE.

           MOVE OTX-LOANDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO TX-LOANDATE  ATX-LOANDATE.

           MOVE OTX-PROCEEDS       TO TX-PROCEEDS  ATX-PROCEEDS.
           MOVE OTX-LNAMT          TO TX-LNAMT     ATX-LNAMT.
           MOVE OTX-FINCHG         TO TX-FINCHG    ATX-FINCHG.
           MOVE OTX-INTCHG         TO TX-INTCHG    ATX-INTCHG.
           MOVE OTX-SERCHG         TO TX-SERCHG    ATX-SERCHG.
           MOVE OTX-EXTCHG         TO TX-EXTCHG    ATX-EXTCHG.
           MOVE OTX-MAINTFEE       TO TX-MAINTFEE  ATX-MAINTFEE.
           MOVE OTX-ORGTERM        TO TX-ORGTERM   ATX-ORGTERM.
           MOVE OTX-INSURED        TO TX-INSURED   ATX-INSURED.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE 0               TO TX-INSANTICERN(SUB)
                                      ATX-INSANTICERN(SUB)
               MOVE 0               TO TX-INSEARNINGS(SUB)
                                      ATX-INSEARNINGS(SUB)
           END-PERFORM.

           MOVE OTX-DLNO            TO TX-DLNO        ATX-DLNO.
           MOVE OTX-DLRECOURSE      TO TX-DLRECOURSE  ATX-DLRECOURSE.
           MOVE OTX-DLPARTRCAMT     TO TX-DLPARTRCAMT ATX-DLPARTRCAMT.
           MOVE OTX-PURBAL          TO TX-PURBAL      ATX-PURBAL.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               MOVE OTX-DLRVPRIN(SUB)   TO TX-DLRVPRIN(SUB)
                                          ATX-DLRVPRIN(SUB)
               MOVE OTX-DLRVCHGS(SUB)    TO TX-DLRVCHGS(SUB)
                                           ATX-DLRVCHGS(SUB)
           END-PERFORM.

           MOVE OTX-DLPARVHELD      TO TX-DLPARVHELD ATX-DLPARVHELD.
           MOVE OTX-DLPARVPAID      TO TX-DLPARVPAID ATX-DLPARVPAID.
           MOVE OTX-DLDISC          TO TX-DLDISC     ATX-DLDISC.
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE OTX-ANTICERN(SUB)   TO TX-ANTICERN(SUB)
                                          ATX-ANTICERN(SUB)
               MOVE OTX-EARNINGS(SUB)      TO TX-EARNINGS(SUB)
                                             ATX-EARNINGS(SUB)
           END-PERFORM.

      *CHANGED TX-PRLNNO TO BE THE 8 DIGIT ACCTNO WITHOUT 01 SEQUENCE
           MOVE OTX-PRLNNO          TO TEST-ACCTNO.
           MOVE TEST-NUMBER         TO TX-PRLNNO    ATX-PRLNNO.      
           MOVE OTX-PRBAL           TO TX-PRBAL     ATX-PRBAL.
           MOVE OTX-TFRBAL          TO TX-TFRBAL    ATX-TFRBAL.
           MOVE OTX-SPRCLASS        TO TX-SPRCLASS  ATX-SPRCLASS.
           MOVE OTX-USERID          TO TX-USERID    ATX-USERID.

           MOVE OTX-POSTDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TX-POSTDATE  ATX-POSTDATE.
           MOVE OTX-POSTTIME        TO TX-POSTTIME  ATX-POSTTIME.

           IF (FILE-OPTION-TR = "Y")
              PERFORM WRITE-TRX1-FILE
           ELSE
              ADD 1 TO ATRX-KEY
              ACCEPT ATX-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ATX-CR
              MOVE X"0A"              TO ATX-LF
              PERFORM WRITE-ATRX-FILE.

           IF ( IO-BAD )
              ADD 1               TO TR1534-RECORD-COUNTER-SKIP
              MOVE "TRX"          TO TR1534-CURRENT-FILE
              MOVE FILE-STAT-DISP TO TR1534-UPDATE-STAT
              MOVE "E"            TO TR1534-UPDATE-TYPE
              PERFORM TR1534-CREATE-FILE-UPDATE
              GO TO IO-ERROR.

       UPGRADE-FILE-TRX-EXIT.
           EXIT.

      *****************************************************************
      *
      * CREATE DONE TAGS FOR EACH LOCAL ASCII FILE THAT WAS SELECTED
      *
      *****************************************************************
       CREATE-DONE-TAGS SECTION.

           IF ( FILE-OPTION-CH          = "A"    ) AND
              ( ACH-DONE-PATH-GROUP NOT = SPACES )
              PERFORM OPEN-ACH-DONE-FILE
              PERFORM CLOSE-ACH-DONE-FILE.

           IF ( FILE-OPTION-TR           = "A"    ) AND
              ( ATRB-DONE-PATH-GROUP NOT = SPACES )
              PERFORM OPEN-ATRB-DONE-FILE
              PERFORM CLOSE-ATRB-DONE-FILE.

           IF ( FILE-OPTION-TR           = "A"    ) AND
              ( ATRC-DONE-PATH-GROUP NOT = SPACES )
              PERFORM OPEN-ATRC-DONE-FILE
              PERFORM CLOSE-ATRC-DONE-FILE.

           IF ( FILE-OPTION-TR           = "A"    ) AND
              ( ATRD-DONE-PATH-GROUP NOT = SPACES )
              PERFORM OPEN-ATRD-DONE-FILE
              PERFORM CLOSE-ATRD-DONE-FILE.

           IF ( FILE-OPTION-TR           = "A"    ) AND
              ( ATRI-DONE-PATH-GROUP NOT = SPACES )
              PERFORM OPEN-ATRI-DONE-FILE
              PERFORM CLOSE-ATRI-DONE-FILE.

           IF ( FILE-OPTION-TR           = "A"    ) AND
              ( ATRJ-DONE-PATH-GROUP NOT = SPACES )
              PERFORM OPEN-ATRJ-DONE-FILE
              PERFORM CLOSE-ATRJ-DONE-FILE.

           IF ( FILE-OPTION-TR           = "A"    ) AND
              ( ATRK-DONE-PATH-GROUP NOT = SPACES )
              PERFORM OPEN-ATRK-DONE-FILE
              PERFORM CLOSE-ATRK-DONE-FILE.

           IF ( FILE-OPTION-TR           = "A"    ) AND
              ( ATRL-DONE-PATH-GROUP NOT = SPACES )
              PERFORM OPEN-ATRL-DONE-FILE
              PERFORM CLOSE-ATRL-DONE-FILE.

           IF ( FILE-OPTION-TR           = "A"    ) AND
              ( ATRP-DONE-PATH-GROUP NOT = SPACES )
              PERFORM OPEN-ATRP-DONE-FILE
              PERFORM CLOSE-ATRP-DONE-FILE.

           IF ( FILE-OPTION-TR           = "A"    ) AND
              ( ATRS-DONE-PATH-GROUP NOT = SPACES )
              PERFORM OPEN-ATRS-DONE-FILE
              PERFORM CLOSE-ATRS-DONE-FILE.

           IF ( FILE-OPTION-TR           = "A"    ) AND
              ( ATRW-DONE-PATH-GROUP NOT = SPACES )
              PERFORM OPEN-ATRW-DONE-FILE
              PERFORM CLOSE-ATRW-DONE-FILE.

           IF ( FILE-OPTION-TR           = "A"    ) AND
              ( ATRX-DONE-PATH-GROUP NOT = SPACES )
              PERFORM OPEN-ATRX-DONE-FILE
              PERFORM CLOSE-ATRX-DONE-FILE.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - M O D U L E
      *
      ******************************************************************
       PRINT-REPORT-MODULE SECTION.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "REPORT"                   TO TR1534-CURRENT-FILE.
           MOVE TR1534-FILE-TYPE---GLOBAL  TO TR1534-FILE-TYPE.

           PERFORM TR1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - -{ SETUP 'REPORT' FILE  }
           MOVE ZEROES                     TO TR1534-REPORT-KEY.

           PERFORM OPEN-REPORT-FILE.
           PERFORM START-REPORT-FILE.

      *=================================================================
       PRINT-REPORT-NEXT-LINE.

           PERFORM READ-REPORT-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PRINT-REPORT-MODULE-END.

           PERFORM TR1534-VPI-DISPLAY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE REPORT-DETAIL-LINE         TO DETAIL-LINE.
           MOVE REPORT-LN-FEED             TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-NEXT-LINE.

      *=================================================================
       PRINT-REPORT-MODULE-END.

           PERFORM CLOSE-REPORT-FILE.
           PERFORM PRINT-RANGES.

      *-----------------------------------------------------------------
       PRINT-REPORT-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R A N G E S
      *
      ******************************************************************
       PRINT-RANGES SECTION.

           MOVE 99 TO LN-COUNT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID NAME                    :" TO R1-DESC
              MOVE ENT-GROUP                            TO R1-FIELD
              MOVE 2                                    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER          :" TO R1-DESC
              MOVE BEG-BRANCH                           TO R1-NUMERIC
              MOVE 2                                    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH NUMBER             :" TO R1-DESC
              MOVE END-BRANCH                           TO R1-NUMERIC
              MOVE 1                                    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE "DATABASE NAME                    :" TO R1-DESC.
           MOVE EXT-ACUSQL-DB-NAME                   TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERVER NAME                      :" TO R1-DESC.
           MOVE EXT-ACUSQL-SERVER-NAME               TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SOURCE PATH                      :" TO R1-DESC.
           MOVE TR1534-SOURCE-PATH                   TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "TARGET PATH                      :" TO R1-DESC.
           MOVE TR1534-TARGET-PATH                   TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "TR-FILE [LP] (Y/N)               :" TO R1-DESC.
           MOVE FILE-OPTION-TR              TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CH-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-CH                TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME OF THE UPGRADE     :" TO R1-DESC.
           MOVE STARTING-TIME                        TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING   TIME OF THE UPGRADE     :" TO R1-DESC.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                            TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       EXIT-PRINT-RANGES.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED LN-COUNT GIVING LN-WK.

      *KC  IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
           IF ( LN-WK GREATER THAN 60 )
              PERFORM WRITE-HEADER-LINES.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED                     TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

      *=================================================================
       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - H E A D E R - L I N E S
      *
      ******************************************************************
       WRITE-HEADER-LINES SECTION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ LINE: 01 }
           ADD 01                          TO PAGE-NUMBER.
           MOVE PAGE-NUMBER                TO H1-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO H1-TIME.
           MOVE HEADER-LINE-1              TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ LINE: 02 }
           MOVE HEADER-LINE-2              TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ LINE: 03 }
           MOVE ALL "-"                    TO PR-REC.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 04                         TO LN-COUNT.
           MOVE 01                         TO LN-FEED.

      *=================================================================
       EXIT-WRITE-HEADER-LINES.
           EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 0 - C R E A T E - D E T A I L - L I N E
      *
      *=================================================================
      *    IN  : TR1534-DETAIL-LINE
      *          TR1534-LN-FEED
      *          TR1534-REPORT-KEY
      *
      *    OUT : TR1534-DETAIL-LINE
      *          TR1534-LN-FEED
      *          TR1534-REPORT-KEY
      *
      *    DESC: WRITE RECORD TO REPORT-FILE USING DETAIL-LINE.
      ******************************************************************
       TR1534-CREATE-DETAIL-LINE SECTION.

            IF ( TR1534-REPORT-KEY = 999999999 )
               GO TO TR1534-CREATE-DETAIL-EXIT.

      *- - - - - - - - - - - - - - - - - - - -{ CREATE 'REPORT' RECORD }
            ADD  01                        TO TR1534-REPORT-KEY.
            MOVE TR1534-DETAIL-LINE        TO REPORT-DETAIL-LINE.
            MOVE TR1534-LN-FEED            TO REPORT-LN-FEED.

            PERFORM WRITE-REPORT-FILE.

            IF ( IO-BAD )
               GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ RESET FIELDS }
            MOVE SPACES                    TO TR1534-DETAIL-LINE.
            MOVE 01                        TO TR1534-LN-FEED.

      *=================================================================
       TR1534-CREATE-DETAIL-EXIT.
            EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 0 - C R E A T E - B R A N C H - H E A D
      *
      *=================================================================
      *    IN  : BR-REC
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE HEADER LINE FOR THE REPORT-FILE.
      ******************************************************************
       TR1534-CREATE-BRANCH-HEAD SECTION.

           MOVE "BRANCH: "                 TO TR1534-D1-BRANCH-PROMPT.
           MOVE BR-NO                      TO TR1534-D1-BRANCH.
           MOVE BR-NAME                    TO TR1534-D1-BRANCH-NAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TR1534-DETAIL-LINE         TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 02                         TO TR1534-LN-FEED.
           PERFORM TR1534-CREATE-DETAIL-LINE.

      *=================================================================
       TR1534-CREATE-B-HEAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 0 - C R E A T E - M A C H I N E - H E A D
      *
      *=================================================================
      *    IN  : TR1534-MACHINE
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE MACHINE LINE FOR THE REPORT-FILE.
      ******************************************************************
       TR1534-CREATE-MACHINE-HEAD SECTION.

           MOVE "MACHINE: "                TO TR1534-D2-MACHINE-PROMPT.
           MOVE TR1534-MACHINE             TO TR1534-D2-MACHINE.
           MOVE "RISC MACHINE"             TO TR1534-D2-MACHINE-NAME.
           MOVE 02                         TO TR1534-LN-FEED.
           PERFORM TR1534-CREATE-DETAIL-LINE.

      *=================================================================
       TR1534-CREATE-M-HEAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 0 - C R E A T E - F I L E - E R R O R
      *
      *=================================================================
      *    IN  : TR1534-CURRECT-FILE
      *          TR1534-FILE-ORDER    [SOURCE/TARGET]
      *          STAT
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE ERROR LINE FOR THE REPORT-FILE.
      ******************************************************************
       TR1534-CREATE-FILE-ERROR SECTION.

           MOVE "FILE: "                   TO TR1534-F1-FILE-PROMPT.

           MOVE SPACES                     TO TR1534-F1-FILE.
           STRING TR1534-CURRENT-FILE, TR1534-CURRENT-MMYY
                  DELIMITED BY "  "      INTO TR1534-F1-FILE.

           IF ( TR1534-FILE-ORDER-SOURCE )
              STRING "ERROR - SOURCE FILE: ",STAT
                  DELIMITED BY "  "      INTO TR1534-F1-FILE-MESSAGE
           ELSE
           IF ( TR1534-FILE-ORDER-TARGET )
              STRING "ERROR - TARGET FILE: ",STAT
                  DELIMITED BY "  "      INTO TR1534-F1-FILE-MESSAGE
           ELSE
              STRING "ERROR - MISC.. FILE: ",STAT
                  DELIMITED BY "  "      INTO TR1534-F1-FILE-MESSAGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TR1534-F1-FILE-MESSAGE     TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01                         TO TR1534-LN-FEED.
           PERFORM TR1534-CREATE-DETAIL-LINE.

      *=================================================================
       TR1534-CREATE-FILE-ER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 0 - C R E A T E - F I L E - U P D A T E
      *
      *=================================================================
      *    IN  : OTR1-KEY
      *          TR1534-CURRENT-FILE
      *          TR1534-CURRENT-MMYY
      *          TR1534-UPDATE-STAT
      *          TR1534-UPDATE-TYPE    [ERROR/SKIP]
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE LINE OF SKIP/ERROR RECORD FOR THE REPORT-FILE.
      ******************************************************************
       TR1534-CREATE-FILE-UPDATE SECTION.

           MOVE "FILE: "                   TO TR1534-F1-FILE-PROMPT.

           MOVE SPACES                     TO TR1534-F1-FILE.
           STRING TR1534-CURRENT-FILE, TR1534-CURRENT-MMYY
                  DELIMITED BY "  "      INTO TR1534-F1-FILE.

           IF ( TR1534-CURRENT-FILE = "CH" )
              MOVE SPACES                  TO TR1534-UPDATE-KEY
              MOVE BR-NO                   TO TR1534-OCH-BRNO
              MOVE OCH-DAY                 TO TR1534-OCH-DAY
              MOVE CH-CODE                 TO TR1534-OCH-CODE
              MOVE CH-USERID               TO TR1534-OCH-USERID
              MOVE CH-TIME                 TO TR1534-OCH-TIME
              MOVE CH-SEQ                  TO TR1534-OCH-SEQ
           ELSE
           IF ( TR1534-CURRENT-FILE = "LPTR" )
              MOVE SPACES                  TO TR1534-UPDATE-KEY
              MOVE OTR1-DAY                TO TR1534-OLPTR1-DAY
              MOVE OTR1-BRNO               TO TR1534-OLPTR1-BRNO
              MOVE OTR1-CODE               TO TR1534-OLPTR1-CODE
              MOVE OTR1-CLASS              TO TR1534-OLPTR1-CLASS.

           IF ( TR1534-UPDATE-TYPE-ERROR )
              MOVE SPACES                  TO TR1534-F1-FILE-MESSAGE
              STRING "ERROR - KEY: ",
                     TR1534-UPDATE-KEY, " ["
                     TR1534-UPDATE-STAT, "]"
                     DELIMITED BY "  "   INTO TR1534-F1-FILE-MESSAGE
           ELSE
              MOVE SPACES                  TO TR1534-F1-FILE-MESSAGE
              STRING "SKIP - KEY: ", TR1534-UPDATE-KEY
                     DELIMITED BY "  "   INTO TR1534-F1-FILE-MESSAGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TR1534-F1-FILE-MESSAGE TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01                         TO TR1534-LN-FEED.
           PERFORM TR1534-CREATE-DETAIL-LINE.

      *=================================================================
       TR1534-CREATE-FILE-UP-END.

           MOVE SPACES                     TO TR1534-UPDATE-STAT
                                              TR1534-UPDATE-TYPE.

      *-----------------------------------------------------------------
       TR1534-CREATE-FILE-UP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 0 - C R E A T E - F I L E - T O T A L S
      *
      *=================================================================
      *    IN  : TR1534-CURRECT-FILE
      *          TR1534-RECORD-COUNTERS
      *          STAT
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE TOTAL LINE FOR THE REPORT-FILE.
      ******************************************************************
       TR1534-CREATE-FILE-TOTALS SECTION.

           MOVE "FILE: "                    TO TR1534-F1-FILE-PROMPT.

           MOVE SPACES                      TO TR1534-F1-FILE.
           STRING TR1534-CURRENT-FILE, TR1534-CURRENT-MMYY
                  DELIMITED BY "  "      INTO TR1534-F1-FILE.

           MOVE "READ: "                    TO
                TR1534-F1-RECORD-READ-PROMPT.
           MOVE TR1534-RECORD-COUNTER-READ  TO TR1534-F1-RECORD-READ.

           MOVE "WRITE: "                   TO
                TR1534-F1-RECORD-WRITE-PROMPT.
           MOVE TR1534-RECORD-COUNTER-WRITE TO TR1534-F1-RECORD-WRITE.

           MOVE "SKIP: "                    TO
                TR1534-F1-RECORD-SKIP-PROMPT.
           MOVE TR1534-RECORD-COUNTER-SKIP  TO TR1534-F1-RECORD-SKIP.

           MOVE "BEG: "                  TO TR1534-F1-BEG-PROMPT.
           MOVE TR1534-FILE-BEG-TIME     TO TR1534-F1-BEG-TIME.
           INSPECT TR1534-F1-BEG-TIME REPLACING ALL "/" BY ":".

           MOVE "END: "                  TO TR1534-F1-END-PROMPT.
           MOVE "Y"                      TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE SPACES                   TO TIME-MILITARY-FG.
           MOVE HH-MM-SS                 TO TR1534-F1-END-TIME.
           INSPECT TR1534-F1-END-TIME REPLACING ALL "/" BY ":".

           MOVE "TOTAL: "                TO TR1534-F1-TOT-PROMPT.
           MOVE TR1534-FILE-BEG-TIME     TO TR1534-TIME-BEG.
           MOVE HH-MM-SS                 TO TR1534-TIME-END.
           PERFORM TR1534-LOAD-TIME-ELAPSED.
           MOVE TR1534-TIME-ELAPSED      TO TR1534-F1-TOT-TIME.
           INSPECT TR1534-F1-TOT-TIME REPLACING ALL "/" BY ":".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    USE POPULATED TR1534-DETAIL-LINE TO UPDATE THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           STRING "     R: ", TR1534-F1-RECORD-READ
                      " W: ", TR1534-F1-RECORD-WRITE
                      " S: ", TR1534-F1-RECORD-SKIP
                  DELIMITED BY SIZE INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           MOVE TR1534-F1-TIMES TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01                          TO TR1534-LN-FEED.
           PERFORM TR1534-CREATE-DETAIL-LINE.

      *=================================================================
       TR1534-CREATE-FILE-T-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 0 - L O A D - F I L E - P A T H
      *
      *=================================================================
      *    IN  : BR-REC
      *          TR1534-FILE-INPUT  [EXP.  XX/XXXXXX OR XX/TR####]
      *          TR1534-FILE-ORDER  [SOURCE/TARGET]
      *          TR1534-FILE-TYPE   [GLOBAL/LOCAL]
      *          TR1534-SOURCE-PATH
      *          TR1534-TARGET-PATH
      *
      *    OUT : TR1534-FILE-PATH
      *          - GLOBAL: /XXX/XXXX/R#/XX/XXXXXX
      *          - LOCAL : /XXX/XXXX/R#/XX####/XX/XXXXXX
      *
      *    DESC: LOAD FILE PATH BASED ON SOURCE/TARGET & GLOBAL/LOCAL.
      ******************************************************************
       TR1534-LOAD-FILE-PATH SECTION.

           MOVE SPACES                     TO TR1534-FILE-PATH.

           IF ( TR1534-FILE-INPUT           = SPACES ) OR
              ( TR1534-FILE-INPUT-DIR       = SPACES ) OR
              ( TR1534-FILE-INPUT-SLASH NOT = "/"    ) OR
              ( TR1534-FILE-INPUT-NAME      = SPACES )
              GO TO TR1534-LOAD-FILE-PATH-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TR1534-FILE-ORDER-SOURCE ) AND
              ( TR1534-FILE-TYPE-GLOBAL  )
              MOVE TR1534-SOURCE-PATH  TO TR1534-FILE-PATH
              MOVE "/"                 TO TR1534-FILE-PATH-SLASH-3
              MOVE TR1534-MACHINE      TO TR1534-FILE-PATH-MACHINE
              MOVE "/"                 TO TR1534-FILE-PATH-SLASH-4
              MOVE TR1534-FILE-INPUT   TO TR1534-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TR1534-FILE-ORDER-SOURCE ) AND
              ( TR1534-FILE-TYPE-LOCAL   )
              MOVE TR1534-SOURCE-PATH    TO TR1534-FILE-PATH
              MOVE "/"                   TO TR1534-FILE-PATH-SLASH-3
      * NEED ORIGINAL OLD BRANCH, MIGHT HAVE RAN FIX
              MOVE BRANCH-MACHINE(BR-NO) TO TR1534-FILE-PATH-MACHINE
              MOVE "/"                 TO TR1534-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO TR1534-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO TR1534-FILE-PATH-LOCAL-DATA-SL
              MOVE TR1534-FILE-INPUT   TO TR1534-FILE-PATH-LOCAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TR1534-FILE-ORDER-TARGET ) AND
              ( TR1534-FILE-TYPE-GLOBAL  )
              MOVE TR1534-TARGET-PATH  TO TR1534-FILE-PATH
              MOVE "/"                 TO TR1534-FILE-PATH-SLASH-3
              MOVE TR1534-MACHINE      TO TR1534-FILE-PATH-MACHINE
              MOVE "/"                 TO TR1534-FILE-PATH-SLASH-4
              MOVE TR1534-FILE-INPUT   TO TR1534-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TR1534-FILE-ORDER-TARGET ) AND
              ( TR1534-FILE-TYPE-LOCAL   )
              MOVE TR1534-TARGET-PATH  TO TR1534-FILE-PATH
              MOVE "/"                 TO TR1534-FILE-PATH-SLASH-3
              MOVE TR1534-MACHINE      TO TR1534-FILE-PATH-MACHINE
              MOVE "/"                 TO TR1534-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO TR1534-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO TR1534-FILE-PATH-LOCAL-DATA-SL
              MOVE TR1534-FILE-INPUT   TO TR1534-FILE-PATH-LOCAL-PATH.

       TR1534-LOAD-FILE-PATH-EXIT.
           EXIT.

      ******************************************************************
      * DUE TO ACU4GL, THE A15 FILE NEEDS TO BE RENAMED (VIA MOVE; MV)
      * SO IT IDENTIFIES THE CORRECT FILE.
      * FOR EXAMPLE, USING THE STFILE, THE A15 FILE OF
      * /US1/DA15/R1/LP/STFILE IS SEEN AS THE A34 STFILE DATABASE TABLE.
      * WHEN TRYING TO OPEN THE A15 FILE IT WILL RESULT IN AN ACU ERROR
      * OF "9D,,2".  FOR THE UPGRADE PROGRAMS TO WORK, NEED TO:
      * 
      *  1. MOVE /US1/DA15/R1/LP/STFILE TO /US1/DA15/R1/LP/OSTFILE
      *  2. OPEN, READ-NEXT, CLOSE ON THE OSTFILE.
      *  3. MOVE /US1/DA15/R1/LP/OSTFILE TO /US1/DA15/R1/LP/STFILE
      * 
      * NEED TO MOVE THE FILE BACK TO ITS ORIGINAL NAME IN CASE THE
      * UPGRADE IS BEING EXECUTED MULTIPLE TIMES (QA TESTING) OR FOR
      * SIDE-BY-SIDE TESTING BETWEEN A15 & A34.
      *
      *=================================================================
      *  BASESH - /US1/DA15/SH/YYFILE
      *  GLOBAL - /US1/DA15/R1/XX/YYFILE
      *  LOCAL  - /US1/DA15/R1/SS####/XX/YYFILE
      *           ....|....1....|....2....|....3
      *
      ******************************************************************
      *
      *    T R 1 5 3 4 - M V 1 5 - O L D - T O - N E W
      *
      *=================================================================
      * IN  : TR1534-MV15-OLD-PATH
      *       TR1534-FILE-TYPE  [B, G, L]
      * OUT : TR1534-MV15-NEW-PATH
      * DESC: EXECUTE SYSTEM MOVE COMMAND TO MOVE OLD FILE TO NEW FILE.
      ******************************************************************
       TR1534-MV15-OLD-TO-NEW SECTION.

           MOVE TR1534-MV15-OLD-PATH TO TR1534-MV15-NEW-PATH.

           IF ( TR1534-FILE-TYPE-BASESH )
              MOVE "O" TO TR1534-MV15-NEW-BASESH-O 
              MOVE TR1534-MV15-OLD-BASESH-FILE TO
                   TR1534-MV15-NEW-BASESH-FILE
           ELSE
           IF ( TR1534-FILE-TYPE-GLOBAL )
              MOVE "O" TO TR1534-MV15-NEW-GLOBAL-O 
              MOVE TR1534-MV15-OLD-GLOBAL-FILE TO
                   TR1534-MV15-NEW-GLOBAL-FILE
           ELSE
              MOVE "O" TO TR1534-MV15-NEW-LOCAL-O 
              MOVE TR1534-MV15-OLD-LOCAL-FILE TO
                   TR1534-MV15-NEW-LOCAL-FILE.

           MOVE MV-CMD               TO SYSTEM-BUF.
           MOVE TR1534-MV15-OLD-PATH TO MV-FILE1.
           MOVE TR1534-MV15-NEW-PATH TO MV-FILE2.

           PERFORM SYSTEM-CALL.

      *=================================================================
       TR1534-MV15-OLD-TO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 4 - M V 1 5 - N E W - T O - O L D
      *
      *=================================================================
      * IN  : TR1534-MV15-NEW-PATH
      *       TR1534-FILE-TYPE  [B, G, L]
      * OUT : TR1534-MV15-OLD-PATH
      * DESC: EXECUTE SYSTEM MOVE COMMAND TO MOVE NEW FILE TO OLD FILE.
      ******************************************************************
       TR1534-MV15-NEW-TO-OLD SECTION.

           MOVE TR1534-MV15-NEW-PATH TO TR1534-MV15-OLD-PATH.

           IF ( TR1534-FILE-TYPE-BASESH )
              MOVE TR1534-MV15-NEW-BASESH-FILE TO
                   TR1534-MV15-OLD-BASESH-FILE
           ELSE
           IF ( TR1534-FILE-TYPE-GLOBAL )
              MOVE TR1534-MV15-NEW-GLOBAL-FILE TO
                   TR1534-MV15-OLD-GLOBAL-FILE
           ELSE
              MOVE TR1534-MV15-NEW-LOCAL-FILE TO
                   TR1534-MV15-OLD-LOCAL-FILE.

           MOVE MV-CMD               TO SYSTEM-BUF.
           MOVE TR1534-MV15-NEW-PATH TO MV-FILE1.
           MOVE TR1534-MV15-OLD-PATH TO MV-FILE2.

           PERFORM SYSTEM-CALL.

      *=================================================================
       TR1534-MV15-NEW-TO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 0 - V P I - D I S P L A Y 
      *
      *=================================================================
      *   IN:  TR1534-VPI-BRNO
      *        TR1534-VPI-FILE-PREFIX
      *        TR1534-VPI-FILE-MMYY
      *        TR1534-VPI-FILE-TYPE
      *        TR1534-VPI-PRU-LP
      *        TR1534-VPI-ROUTE-BUF
      *        TR1534-VPI-SLEEP-COUNTER
      *        TR1534-VPI-SLEEP-COUNTER-MAX
      *        TR1534-VPI-TICKER
      *        TR1534-VPI-TIME-EDIT
      *
      *  OUT:  TR1534-VPI-SLEEP-COUNTER
      *        TR1534-VPI-TICKER
      *
      * DESC:  TR1534 VIDEO PROGRESS INDICATOR (VPI)
      *        DISPLAYS TO THE SCREEN THE BRANCH NUMBER & FILE THE
      *        PROGRAM IS CURRENTLY CONVERTING.
      *
      ******************************************************************
       TR1534-VPI-DISPLAY SECTION.

           IF ( TR1534-VPI-SLEEP-COUNTER 
              < TR1534-VPI-SLEEP-COUNTER-MAX )
              ADD 1 TO TR1534-VPI-SLEEP-COUNTER
              GO TO TR1534-VPI-DISPLAY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BEGIN TO CREATE TR1534-VPI-DISPLAY-BUF
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES                     TO TR1534-VPI-SLEEP-COUNTER.
           MOVE SPACES                     TO TR1534-VPI-DISPLAY-BUF.

           MOVE "("                        TO TR1534-VPI-DISPLAY-LEFT.
           MOVE ")"                        TO TR1534-VPI-DISPLAY-RIGHT.

           IF ( TR1534-VPI-TICKER = "-" )
              MOVE "\"                     TO TR1534-VPI-DISPLAY-TICKER
                                              TR1534-VPI-TICKER
           ELSE
           IF ( TR1534-VPI-TICKER = "\" )
              MOVE "|"                     TO TR1534-VPI-DISPLAY-TICKER
                                              TR1534-VPI-TICKER
           ELSE
           IF ( TR1534-VPI-TICKER = "|" )
              MOVE "/"                     TO TR1534-VPI-DISPLAY-TICKER
                                              TR1534-VPI-TICKER
           ELSE
              MOVE "-"                     TO TR1534-VPI-DISPLAY-TICKER
                                              TR1534-VPI-TICKER.

           IF ( TR1534-VPI-FILE-TYPE-LOCAL )
              MOVE TR1534-VPI-BRNO         TO TR1534-VPI-DISPLAY-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET THE FILE NAME IN THE DISPLAY
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "***FILE NOT FOUND***"     TO TR1534-VPI-DISPLAY-FILE

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ LOCAL  }
           IF ( TR1534-VPI-FILE-CH )
              MOVE SPACES                  TO TR1534-VPI-DISPLAY-FILE
              STRING "CHANGES (CH) ", 
                     TR1534-VPI-FILE-MMYY
                     DELIMITED BY SIZE   INTO TR1534-VPI-DISPLAY-FILE
           ELSE
           IF ( TR1534-VPI-FILE-TR )
              MOVE SPACES                  TO TR1534-VPI-DISPLAY-FILE
              STRING "TRANSACTION (LP) ", 
                     TR1534-VPI-FILE-MMYY
                     DELIMITED BY SIZE   INTO TR1534-VPI-DISPLAY-FILE
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ MISC }
           IF ( TR1534-VPI-FILE-WK1 )
              MOVE "WORK FILE 1 (WK1)"
                TO TR1534-VPI-DISPLAY-FILE
           ELSE
           IF ( TR1534-VPI-FILE-WK2 )
              MOVE "WORK FILE 2 (WK2)"
                TO TR1534-VPI-DISPLAY-FILE
           ELSE
           IF ( TR1534-VPI-FILE-REPORT )
              MOVE "TR1534 REPORT (REPORT)"
                TO TR1534-VPI-DISPLAY-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TR1534-VPI-TIME-EDIT       TO TR1534-VPI-DISPLAY-TIME.
           CALL "C$JUSTIFY" USING TR1534-VPI-DISPLAY-TEXT "L".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SENDING THE 'PROGRESS INDICTATOR' LINE DISPLAY TO THE SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TR1534-VPI-ROUTE-MENU-DRIVEN     ) AND
              ( TR1534-VPI-QUEUE-PRINTER-OR-VIEW )
              MOVE TR1534-VPI-DISPLAY-BUF  TO WR-BUF
              MOVE TR1534-VPI-DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      *=================================================================
       TR1534-VPI-DISPLAY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 0 - V P I - S T A R T
      *
      *=================================================================
      *   IN:  TR1534-CURRENT-FILE
      *        TR1534-CURRENT-MMYY
      *        TR1534-FILE-TYPE
      *        TR1534-PRU-LP
      *
      *  OUT:  TR1534-VPI WORKERS
      *
      * DESC:  SETTING THE VIDEO PROGRESS INDICATOR (VPI) WORKERS FOR
      *        THE START OF FILE UPGRADE.
      *
      ******************************************************************
       TR1534-VPI-START SECTION.

           IF ( TR1534-FILE-TYPE-LOCAL )
              MOVE BR-NO                   TO TR1534-VPI-BRNO
           ELSE
              MOVE ZEROES                  TO TR1534-VPI-BRNO.

           MOVE TR1534-CURRENT-FILE        TO TR1534-VPI-FILE-PREFIX.
           MOVE TR1534-CURRENT-MMYY        TO TR1534-VPI-FILE-MMYY.
           MOVE TR1534-FILE-TYPE           TO TR1534-VPI-FILE-TYPE.
           MOVE TR1534-PRU-LP              TO TR1534-VPI-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO TR1534-VPI-ROUTE-BUF.
           MOVE 9999                       TO TR1534-VPI-SLEEP-COUNTER.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO TR1534-VPI-TIME-EDIT.

      *=================================================================
       TR1534-VPI-START-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - L O A D - F I L E - B E G - T I M E
      *
      *=================================================================
      *    IN  : NOTHING
      *    OUT : TR1534-FILE-BEG-TIME
      *    DESC: LOAD THE BEGINNING TIME OF THE UPGRADE OF FILE
      ******************************************************************
       TR1534-LOAD-FILE-BEG-TIME SECTION.

           MOVE "Y"                        TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE SPACES                     TO TIME-MILITARY-FG.
           MOVE HH-MM-SS                   TO TR1534-FILE-BEG-TIME.

      *=================================================================
       TR1534-LOAD-FILE-B-T-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - L O A D - T I M E - E L A P S E D
      *
      *=================================================================
      *    IN  : TR1534-TIME-BEG
      *          TR1534-TIME-END
      *
      *    OUT : TR1534-TIME-ELAPSED
      *
      *    DESC: USED TO GET THE ELAPSED TIME BETWEEN BEGIN AND END
      *
      *    NOTE: IF THE TIME CROSSED OVER MIDNIGHT, NEED TO ADD 24 HOURS
      *          TO THE END TIME SO TO CALCULATE THE ELAPSED TIME.
      *
      *=================================================================
      *    CALCULATION IS BASED ON CONVERTING THE TIME INTO SECONDS:
      *      1 HOUR   = 3600 SECONDS
      *      1 MINUTE = 60 SECONDS
      *      1 SECOND = 1 SECOND
      *
      *      02:34:56  = 9296
      *      02 * 3600 = 7200
      *      34 * 60   = 2040
      *      56 * 1    = 56
      *
      *      9296 = 02:34:56
      *      02 = 9296 / 3600
      *      34 = (9296 - (3600 * (9296/3600))) / 60
      *      56 = (9296 - (3600 * (9296/3600)) -
      *           ( 60 * ((9296 - (3600 * (9296/3600)))
      *
      *      9296 = 02:34:56
      *      02 = 9296 / 3600
      *      34 = (9296 - (3600 * [2]) / 60
      *      56 = (9296 - (3600 * [2]) - (60 * [34])
      *            9296 - (  7200    ) - (  2040   )
      *
      ******************************************************************
       TR1534-LOAD-TIME-ELAPSED SECTION.

           IF ( TR1534-TIME-BEG-HH > TR1534-TIME-END-HH )
              ADD 24 TO TR1534-TIME-END-HH.

           COMPUTE TR1534-TIME-SECONDS = (( TR1534-TIME-END-HH * 3600 )
                                       +  ( TR1534-TIME-END-MM * 60   )
                                       +    TR1534-TIME-END-SS         )
                                       - (( TR1534-TIME-BEG-HH * 3600 )
                                       +  ( TR1534-TIME-BEG-MM * 60   )
                                       +    TR1534-TIME-BEG-SS        ).

           COMPUTE TR1534-TIME-ELAPSED-HH =
                   TR1534-TIME-SECONDS / 3600.

           COMPUTE TR1534-TIME-ELAPSED-MM =
                   ( TR1534-TIME-SECONDS -
                    (3600 * TR1534-TIME-ELAPSED-HH))
                   / 60.

           COMPUTE TR1534-TIME-ELAPSED-SS =
                   TR1534-TIME-SECONDS -
                   (3600 * TR1534-TIME-ELAPSED-HH) -
                   (60 * TR1534-TIME-ELAPSED-MM).

      *=================================================================
       TR1534-LOAD-TIME-ELAP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E X T R A C T - R O U T I N E
      *
      ******************************************************************
       EXTRACT-ROUTINE SECTION.

           MOVE MKTEMP-BUF                 TO MSEXCEL-PATH.
           MOVE "TR1534-MSEXCEL-"          TO MSEXCEL-PATH-PREFIX.
           MOVE PROCESS-ID-BUF             TO MSEXCEL-PATH-SUFFIX.
           INSPECT MSEXCEL-PATH-SUFFIX REPLACING ALL SPACES BY "9".
           MOVE ".TXT"                     TO MSEXCEL-PATH-EXT.
           CALL "C$TOLOWER" USING MSEXCEL-PATH-EXT, VALUE 04.
           PERFORM OPEN-MSEXCEL-FILE-OUTPUT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO SORT-FILENAME
                          SORT-FILEPATH
                          SORT-STATE
           MOVE ZEROES TO SORT-BRNO.

           PERFORM CLOSE-SORT-FILE.
           PERFORM OPEN-SORT-FILE.
           PERFORM START-SORT-FILE.

      *=================================================================
       EXTRACT-ROUTINE-READ-NEXT.

           PERFORM READ-SORT-FILE-NEXT.

           IF ( IO-BAD )
              GO TO EXTRACT-ROUTINE-END.

           MOVE SORT-TIME-BEG              TO EDF-SORT-TIME-BEG.
           MOVE SORT-TIME-END              TO EDF-SORT-TIME-END.
           MOVE SORT-TIME-ELAPSED          TO EDF-SORT-TIME-ELAPSED.

           MOVE SPACES                     TO MSEXCEL-REC.

           STRING SORT-FILENAME,         HEX-TAB,
                  SORT-FILEPATH,         HEX-TAB,
                  SORT-STATE,            HEX-TAB,
                  SORT-BRNO,             HEX-TAB,
                  SORT-RECS-READ,        HEX-TAB,
                  SORT-RECS-WRITE,       HEX-TAB,
                  SORT-RECS-SKIP,        HEX-TAB,
                  SORT-RECS-SKIP-F-G,    HEX-TAB,
                  EDF-SORT-TIME-BEG,     HEX-TAB,
                  EDF-SORT-TIME-END,     HEX-TAB,
                  EDF-SORT-TIME-ELAPSED, HEX-CARRIAGE-RETURN
                  DELIMITED BY "  " INTO MSEXCEL-REC.

           PERFORM WRITE-MSEXCEL-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO EXTRACT-ROUTINE-READ-NEXT.

      *=================================================================
       EXTRACT-ROUTINE-END.
           PERFORM CLOSE-MSEXCEL-FILE.
           PERFORM CLOSE-SORT-FILE.
           PERFORM REMOVE-SORT-FILE.

       EXTRACT-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - S O R T - M O D U L E
      *
      *=================================================================
      * IN  : HOLD-SORT-KEY
      *       TR1534-RECORD-COUNTER-READ
      *       TR1534-RECORD-COUNTER-WRITE
      *       TR1534-RECORD-COUNTER-SKIP
      *       TR1534-TIME-BEG ***
      *       TR1534-TIME-END ***
      *       TR1534-TIME-ELAPSED ***
      *       *** = SET WITHIN TR1534-CREATE-FILE-TOTALS SECTION
      * OUT : NOTHING
      * DESC: UPDATE THE SORT-FILE.  THE SORT FILE IS USED TO 
      *       CREATE THE MSEXCEL-FILE WHICH CAN BE IMPORTED INTO
      *       MS EXCEL.
      *
      *       THIS CREATES A DETAIL RECORD FOR EACH FILE UPGRADED.
      *       IT ALSO CREATES/UPDATES A TOTAL RECORD FOR ALL
      *       FILES (GLOBAL, ALL MACHINES; LOCAL, ALL BRANCHES).
      *
      *       FOR TIMES, THE INPUT IS IN A STANDARD FORMAT (HH:MM:SS)
      *       BUT BEFORE UPDATED IN THE SORT-REC THEY NEED TO BE
      *       CONVERTED.  THEY NEED TO BE CONVERTED TO SECONDS THEN
      *       DIVIDED BY 86400 (TOTAL OF SECOND FOR 24 HOURS).
      *       FOR EXAMPLE, 12:34:56 IS 45296 SECONDS WHICH DIVIDED BY
      *       86400 SECONDS IS 0.5242592 WHICH WITHIN MS EXCEL YOU CAN
      *       FORMAT THE WHOLE COLUMN TO A CUSTOM FORMAT OF HH:MM:SS.
      ******************************************************************
       UPDATE-SORT-MODULE SECTION.

           CALL "C$TOUPPER" USING HOLD-SORT-STATE, VALUE 02.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ FILE DETAIL }
           MOVE SPACES TO SORT-REC.
           MOVE HOLD-SORT-FILENAME          TO SORT-FILENAME.
           MOVE HOLD-SORT-FILEPATH          TO SORT-FILEPATH.
           MOVE HOLD-SORT-STATE             TO SORT-STATE.
           MOVE HOLD-SORT-BRNO              TO SORT-BRNO.
           MOVE TR1534-RECORD-COUNTER-READ  TO SORT-RECS-READ.
           MOVE TR1534-RECORD-COUNTER-WRITE TO SORT-RECS-WRITE.
           MOVE TR1534-RECORD-COUNTER-SKIP  TO SORT-RECS-SKIP.
           MOVE TR1534-RECORD-COUNTER-SKIP-F-G
                                            TO SORT-RECS-SKIP-F-G.

           MOVE TR1534-TIME-BEG             TO WSS-MSEXCEL-TIME-INPUT.
           PERFORM MISC-CONVERT-MSEXCEL-TIME.
           MOVE WSS-MSEXCEL-TIME-OUTPUT     TO SORT-TIME-BEG.

           MOVE TR1534-TIME-END             TO WSS-MSEXCEL-TIME-INPUT.
           PERFORM MISC-CONVERT-MSEXCEL-TIME.
           MOVE WSS-MSEXCEL-TIME-OUTPUT     TO SORT-TIME-END.

           MOVE TR1534-TIME-ELAPSED         TO WSS-MSEXCEL-TIME-INPUT.
           PERFORM MISC-CONVERT-MSEXCEL-TIME.
           MOVE WSS-MSEXCEL-TIME-OUTPUT     TO SORT-TIME-ELAPSED.

           PERFORM WRITE-SORT-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ FILE TOTAL }
           MOVE HOLD-SORT-FILENAME         TO SAVE-SORT-FILENAME.
           MOVE SPACES                     TO SAVE-SORT-FILEPATH.
           MOVE SPACES                     TO SAVE-SORT-STATE.
           MOVE 9999                       TO SAVE-SORT-BRNO.
           MOVE SAVE-SORT-KEY              TO SORT-KEY.
           PERFORM READ-SORT-FILE.

           IF ( IO-BAD )
              PERFORM UPDATE-SORT-FILE-WRITE.

           PERFORM UPDATE-SORT-REWRITE.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ BRANCH TOTAL }
           IF ( HOLD-SORT-BRNO = ZEROES )
              GO TO UPDATE-SORT-GRAND-TOTAL.

           MOVE "~ALL~"                    TO SAVE-SORT-FILENAME.
           MOVE "~BRANCH TOTAL~"           TO SAVE-SORT-FILEPATH.
           MOVE SPACES                     TO SAVE-SORT-STATE.
           MOVE HOLD-SORT-BRNO             TO SAVE-SORT-BRNO.
           MOVE SAVE-SORT-KEY              TO SORT-KEY.
           PERFORM READ-SORT-FILE.

           IF ( IO-BAD )
              PERFORM UPDATE-SORT-FILE-WRITE.

           PERFORM UPDATE-SORT-REWRITE.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ STATE TOTAL }
           IF ( HOLD-SORT-STATE = SPACES )
              GO TO UPDATE-SORT-GRAND-TOTAL.

           MOVE "~~ALL~~"                  TO SAVE-SORT-FILENAME.
           MOVE "~~STATE TOTAL~~"          TO SAVE-SORT-FILEPATH.
           MOVE HOLD-SORT-STATE            TO SAVE-SORT-STATE.
           MOVE ZEROES                     TO SAVE-SORT-BRNO.
           MOVE SAVE-SORT-KEY              TO SORT-KEY.
           PERFORM READ-SORT-FILE.

           IF ( IO-BAD )
              PERFORM UPDATE-SORT-FILE-WRITE.

           PERFORM UPDATE-SORT-REWRITE.

      *--------------------------------------------------{ GRAND TOTAL }
       UPDATE-SORT-GRAND-TOTAL.

           MOVE "~~~ALL~~~"                TO SAVE-SORT-FILENAME.
           MOVE "~~~GRAND TOTAL~~~"        TO SAVE-SORT-FILEPATH.
           MOVE "~~"                       TO SAVE-SORT-STATE.
           MOVE 9999                       TO SAVE-SORT-BRNO.
           MOVE SAVE-SORT-KEY              TO SORT-KEY.
           PERFORM READ-SORT-FILE.

           IF ( IO-BAD )
              PERFORM UPDATE-SORT-FILE-WRITE.

           PERFORM UPDATE-SORT-REWRITE.

      *=================================================================
       UPDATE-SORT-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - S O R T - R E W R I T E
      *
      *=================================================================
      * IN  : SORT-REC
      *       TR1534-RECORD-COUNTER-READ
      *       TR1534-RECORD-COUNTER-WRITE
      *       TR1534-RECORD-COUNTER-SKIP
      *       TR1534-TIME-BEG
      *       TR1534-TIME-END
      *       TR1534-TIME-ELAPSED
      * OUT : SORT-REC
      * DESC: STANDARD ROUTINE TO ADD TO TOTALS THEN REWRITE EXISTING
      *       SORT-REC. 
      ******************************************************************
       UPDATE-SORT-REWRITE SECTION.

           COMPUTE SORT-RECS-READ = SORT-RECS-READ
                                  + TR1534-RECORD-COUNTER-READ.
           COMPUTE SORT-RECS-WRITE = SORT-RECS-WRITE
                                  + TR1534-RECORD-COUNTER-WRITE.
           COMPUTE SORT-RECS-SKIP = SORT-RECS-SKIP
                                  + TR1534-RECORD-COUNTER-SKIP.
           COMPUTE SORT-RECS-SKIP-F-G = SORT-RECS-SKIP-F-G
                                  + TR1534-RECORD-COUNTER-SKIP-F-G.


      *KC  MOVE TR1534-TIME-BEG             TO WSS-MSEXCEL-TIME-INPUT.
      *KC  PERFORM MISC-CONVERT-MSEXCEL-TIME.
      *KC  COMPUTE SORT-TIME-BEG = SORT-TIME-BEG
      *KC                        + WSS-MSEXCEL-TIME-OUTPUT.

      *KC  MOVE TR1534-TIME-END             TO WSS-MSEXCEL-TIME-INPUT.
      *KC  PERFORM MISC-CONVERT-MSEXCEL-TIME.
      *KC  COMPUTE SORT-TIME-END = SORT-TIME-END
      *KC                        + WSS-MSEXCEL-TIME-OUTPUT.

           MOVE TR1534-TIME-ELAPSED         TO WSS-MSEXCEL-TIME-INPUT.
           PERFORM MISC-CONVERT-MSEXCEL-TIME.
           COMPUTE SORT-TIME-ELAPSED = SORT-TIME-ELAPSED
                                     + WSS-MSEXCEL-TIME-OUTPUT.

           PERFORM REWRITE-SORT-FILE.

      *=================================================================
       UPDATE-SORT-REWRITE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - S O R T - W R I T E 
      *
      *=================================================================
      * IN  : SAVE-SORT-KEY
      * OUT : SORT-REC
      * DESC: STANDARD ROUTINE TO CREATE (WRITE) SORT-REC
      ******************************************************************
       UPDATE-SORT-FILE-WRITE SECTION.

           MOVE SPACES                     TO SORT-REC.
           MOVE SAVE-SORT-KEY              TO SORT-KEY.
           MOVE ZEROES                     TO SORT-RECS-READ
                                              SORT-RECS-WRITE
                                              SORT-RECS-SKIP
                                              SORT-TIME-BEG
                                              SORT-TIME-END
                                              SORT-TIME-ELAPSED.
           PERFORM WRITE-SORT-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

      *=================================================================
       UPDATE-SORT-FILE-WRIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.
     
      *    MOVE EXT-CONAME-CONAME          TO TR1534-CONAME.
           MOVE EXT-CONAME-SYSDATE         TO TR1534-SYSDATE.
      *    DISPLAY TR1534-SCREEN         UPON TCLP-MAIN-WINDOW-HANDLE.
           DISPLAY TR1534-SCREEN         UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TR1534-FORM-FIELDS         TO WR-BUF.
           MOVE TR1534-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.
     
      *=================================================================
       FORM-RESET-EXIT.
           EXIT.
     
      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.
     
           MOVE TR1534-HELP--FILE          TO HELP-LINK-FILE.
           MOVE TR1534-HELP--DIR           TO HELP-LINK-DIR.
     
      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.
     
      ******************************************************************
      *
      *    V D U - C O N V E R T - F I E L D
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
     
           MOVE STAT---GOOD                TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.
     
           EVALUATE TRUE
              COPY "LIBUP/TR1534_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.
     
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBLP/LPCHCL.CPY".

       COPY "LIBLP/LPTRBCL.CPY".
       COPY "LIBLP/LPTRCCL.CPY".
       COPY "LIBLP/LPTRDCL.CPY".
       COPY "LIBLP/LPTRICL.CPY".
       COPY "LIBLP/LPTRJCL.CPY".
       COPY "LIBLP/LPTRKCL.CPY".
       COPY "LIBLP/LPTRLCL.CPY".
       COPY "LIBLP/LPTRPCL.CPY".
       COPY "LIBLP/LPTRSCL.CPY".
       COPY "LIBLP/LPTRWCL.CPY".
       COPY "LIBLP/LPTRXCL.CPY".
      * ASCII CLEARS
       COPY "LIBUP/UPATRBCL.CPY".
       COPY "LIBUP/UPATRCCL.CPY".
       COPY "LIBUP/UPATRDCL.CPY".
       COPY "LIBUP/UPATRICL.CPY".
       COPY "LIBUP/UPATRJCL.CPY".
       COPY "LIBUP/UPATRKCL.CPY".
       COPY "LIBUP/UPATRLCL.CPY".
       COPY "LIBUP/UPATRPCL.CPY".
       COPY "LIBUP/UPATRSCL.CPY".
       COPY "LIBUP/UPATRWCL.CPY".
       COPY "LIBUP/UPATRXCL.CPY".
       COPY "LIBUP/UPACHCL.CPY".

      ******************************************************************
      *
      *     M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPH SECTION.
      *=================================================================
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *=================================================================
       INVALID-FILE-PATH-MESSAGE.
           MOVE MESSAGE-INVALID-FILE-PATH TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       INVALID-SAME-PATHS-MESSAGE.
           MOVE MESSAGE-INVALID-SAME-PATHS TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       INVALID-YNC-MESSAGE.
           MOVE MESSAGE-INVALID-YNC TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
      * IN  : WSS-MSEXCEL-TIME-INPUT
      * OUT : WSS-MSEXCEL-TIME-OUTPUT
      * DESC: CONVERT INPUT TIME IN STANDARD FORMAT (HH:MM:SS) TO 
      *       TO MS EXCEL DATE FORMAT. IT NEEDS TO BE CONVERTED TO
      *       SECONDS THEN DIVIDED BY 86400 (TOTAL OF SECOND FOR
      *       24 HOURS).  FOR EXAMPLE, 12:34:56 IS 45296 SECONDS WHICH
      *       DIVIDED BY 86400 SECONDS IS 0.5242592 WHICH WITHIN
      *       MS EXCEL YOU CAN FORMAT THE WHOLE COLUMN TO A
      *       CUSTOM FORMAT OF HH:MM:SS.
      *-----------------------------------------------------------------
       MISC-CONVERT-MSEXCEL-TIME.

           COMPUTE WSS-MSEXCEL-TIME-OUTPUT =
                     (( WSS-MSEXCEL-TIME-INPUT-HH * 3600 )
                   +  ( WSS-MSEXCEL-TIME-INPUT-MM * 60   )
                   +    WSS-MSEXCEL-TIME-INPUT-SS         )
                   / 86400.

      *=================================================================
       SEND-CUSTOMER-LINE.

           MOVE 1 TO GB-KEY.
      *KC> PERFORM OPEN-GB-FILE.
      *KC> PERFORM READ-GB-FILE.
      *KC> PERFORM CLOSE-GB-FILE.

      *KC> IF ( IO-BAD )
              MOVE "**N/A**" TO GB-UUCP-HOME.

           MOVE "[ "         TO CUSTOMER-RIGHT-SIDE.
           MOVE GB-UUCP-HOME TO CUSTOMER-ID.
           MOVE " ]"         TO CUSTOMER-LEFT-SIDE.

           MOVE CUSTOMER-LINE-BUF          TO WR-BUF.
           MOVE CUSTOMER-LINE-LIST         TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-FIL.

           MOVE "FIL"                      TO GETENV-BUF.
           PERFORM GETENV-CALL.

           MOVE SPACES                     TO TR1534-FILE-PATH.
           STRING "/", GETENV-BUF
                  DELIMITED BY "  "      INTO TR1534-FILE-PATH.
           MOVE TR1534-FILE-PATH-MACHINE   TO TR1534-MACHINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHECK TO SEE IF THE SOURCE & TARGET PATHS ARE SET FROM BATCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TR1534-SOURCE-PATH = SPACES )
              MOVE TR1534-FILE-PATH        TO TR1534-SOURCE-PATH
           ELSE
              MOVE TR1534-SOURCE-PATH      TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE TR1534-FILE-PATH     TO TR1534-SOURCE-PATH.
     
           IF ( TR1534-TARGET-PATH = SPACES )
              MOVE TR1534-FILE-PATH        TO TR1534-TARGET-PATH
           ELSE
              MOVE TR1534-TARGET-PATH      TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE TR1534-FILE-PATH     TO TR1534-TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CURRENT FIL: "            TO FIL-PROMPT.
           MOVE TR1534-FILE-PATH           TO FIL-INFO.


           MOVE FIL-BUF                    TO WR-BUF.
           MOVE FIL-LIST                   TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND                     TO WR-BUF.
           MOVE LEGEND-LIST                TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-MACHINE.

           MOVE "ONLY FOR MACHINE: " TO MACHINE-PROMPT.
           MOVE TR1534-MACHINE       TO MACHINE-NAME.

           MOVE MACHINE-BUF                TO WR-BUF.
           MOVE MACHINE-LIST               TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-STARTING-TIME.

           PERFORM GET-TIME.
           MOVE "[ "      TO STARTING-TIME-RIGHT.
           MOVE TIME-EDIT TO STARTING-TIME.
           MOVE " ]"      TO STARTING-TIME-LEFT.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE STARTING-TIME-BUF       TO WR-BUF
              MOVE STARTING-TIME-LIST      TO WR-LIST
              PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-TITLE.
           MOVE "S  A300TITLE:FR."                       TO VDU.
           MOVE "[ A15 TO A30 --- TRANSACTION FILES   ]" TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       MISC-TRACE-END-PROGRAM.

           MOVE ZEROES TO EXT-TCLP-TRACE-LEVEL.
           MOVE SPACES TO EXT-TCLP-TRACE-FORM-PATH
                          EXT-TCLP-TRACE-MESS.

           MOVE "N"    TO EXT-TCLP-TRACE-SKIP-YN.   *> LOG EVENT
           MOVE "N"    TO EXT-TCLP-TRACE.           *> DISABLE TRACE

      *-----------------------------------------------------------------
       MISC-TRACE-FILE-START.

           STRING "+ FILE: ", TR1534-CURRENT-FILE,
                  "[", BR-NO, "] {", TRFILE-MMYY, "}"
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      *-----------------------------------------------------------------
      *    FORCE UPGRADE PROGRAM TO USE TRACE LOGIC; LOG SETTINGS
      *-----------------------------------------------------------------
       MISC-TRACE-INITIALIZATION.

           MOVE ZEROES TO EXT-TCLP-TRACE-LEVEL.
           MOVE SPACES TO EXT-TCLP-TRACE-FORM-PATH
                          EXT-TCLP-TRACE-MESS.

           MOVE "N"    TO EXT-TCLP-TRACE-SKIP-YN.   *> LOG EVENT
           MOVE "Y"    TO EXT-TCLP-TRACE.           *> ENABLE NON-BRANCH

           MOVE ALL "="                    TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.

           STRING "OTIS SESSION STARTED /", FORM-REL,
                  " | ", DATE-MMSDDSYY
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "DATABASE NAME = ", EXT-ACUSQL-DB-NAME
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "SQL SERVER    = ", EXT-ACUSQL-SERVER-NAME
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "BATCH CODE    = ", EXT-EOBUF-PCODE
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           MOVE EXT-EOBUF-DATE1            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.
           STRING "BEG DATE      = ", DATE-MMSDDSYY
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           MOVE EXT-EOBUF-DATE2            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.
           STRING "END DATE      = ", DATE-MMSDDSYY
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "SOURCE PATH   = ", TR1534-SOURCE-PATH
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "TARGET PATH   = ", TR1534-TARGET-PATH
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "BEG BRANCH    = ", BEG-BRANCH
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "END BRANCH    = ", END-BRANCH
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "GROUP FLAG    = ", GROUP-FLAG
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "ENT GROUP     = ", ENT-GROUP
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBLP/LPTRB1IN.CPY" .
       COPY "LIBLP/LPTRC1IN.CPY" .
       COPY "LIBLP/LPTRD1IN.CPY" .
       COPY "LIBLP/LPTRI1IN.CPY" .
       COPY "LIBLP/LPTRJ1IN.CPY" .
       COPY "LIBLP/LPTRK1IN.CPY" .
       COPY "LIBLP/LPTRL1IN.CPY" .
       COPY "LIBLP/LPTRP1IN.CPY" .
       COPY "LIBLP/LPTRS1IN.CPY" .
       COPY "LIBLP/LPTRW1IN.CPY" .
       COPY "LIBLP/LPTRX1IN.CPY" .
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY" .
       COPY "LIBLP/LPTRCGS_SQL.CPY" .
       COPY "LIBLP/LPTRDGS_SQL.CPY" .
       COPY "LIBLP/LPTRIGS_SQL.CPY" .
       COPY "LIBLP/LPTRJGS_SQL.CPY" .
       COPY "LIBLP/LPTRKGS_SQL.CPY" .
       COPY "LIBLP/LPTRLGS_SQL.CPY" .
       COPY "LIBLP/LPTRPGS_SQL.CPY" .
       COPY "LIBLP/LPTRSGS_SQL.CPY" .
       COPY "LIBLP/LPTRWGS_SQL.CPY" .
       COPY "LIBLP/LPTRXGS_SQL.CPY" .

      * FILES FOR SQL ASCII
       COPY "LIBUP/UPATRBI.CPY".
       COPY "LIBUP/UPATRCI.CPY".
       COPY "LIBUP/UPATRDI.CPY".
       COPY "LIBUP/UPATRII.CPY".
       COPY "LIBUP/UPATRJI.CPY".
       COPY "LIBUP/UPATRKI.CPY".
       COPY "LIBUP/UPATRLI.CPY".
       COPY "LIBUP/UPATRPI.CPY".
       COPY "LIBUP/UPATRSI.CPY".
       COPY "LIBUP/UPATRWI.CPY".
       COPY "LIBUP/UPATRXI.CPY".
       COPY "LIBUP/UPACHI.CPY".

       COPY "LIBSP/SPIOSORT.CPY".
      *=================================================================
      * COPYMEMBER: LIBGB/GBBRRN
      *=================================================================
       OPEN-OBR-FILE.
           PERFORM OPEN-IT.
           MOVE OBR-PATH TO E-FILE.
           OPEN INPUT OBR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBR-FILE.
           IF IO-FG = 7
              CLOSE OBR-FILE
              GO TO OPEN-OBR-FILE.

      *-----------------------------------------------------------------
       READ-OBR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           READ OBR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBR-FILE-NEXT.
      *-----------------------------------------------------------------
       START-OBR-FILE.
           PERFORM START-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           START OBR-FILE KEY NOT < OBR-KEY.

      *-----------------------------------------------------------------
       CLOSE-OBR-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBR-FILE.

      *=================================================================
      *    SOURCE FILE - CH - NEEDED TO BE HARDCODED 
      *=================================================================
      * COPYMEMBER: LIBLP/LPCHR
       OPEN-OCH-FILE.
           PERFORM OPEN-IT.
           MOVE OCH-PATH TO E-FILE.
           OPEN INPUT OCH-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCH-FILE.
           IF IO-FG = 7
              CLOSE OCH-FILE
              GO TO OPEN-OCH-FILE.
       READ-OCH-FILE.
           PERFORM READ-IT.
           MOVE OCH-PATH TO E-FILE.
           READ OCH-FILE.
           IF IO-FG = 8
              GO TO READ-OCH-FILE.
       CLOSE-OCH-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCH-FILE.

      *=================================================================
      *    SOURCE FILE - TR (LP) - NEEDED TO BE HARDCODED 
      *=================================================================
       OPEN-OTR1-FILE.
           PERFORM OPEN-IT.
           MOVE OTR-PATH TO E-FILE.
           OPEN INPUT OTR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTR1-FILE.
           IF IO-FG = 7
              CLOSE OTR-FILE
              GO TO OPEN-OTR1-FILE.
       READ-OTR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           READ OTR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTR1-FILE-NEXT.
       READ-OTR1-FILE.
           PERFORM READ-IT.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           READ OTR-FILE.
           IF IO-FG = 8
              GO TO READ-OTR1-FILE.
       START-OTR1-FILE.
           PERFORM START-IT.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           START OTR-FILE KEY NOT < OTR1-KEY.
       CLOSE-OTR1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OTR-FILE.


      *=================================================================
       OPEN-REPORT-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE TR1534-REPORT-IFN TO E-FILE.
           OPEN OUTPUT REPORT-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-REPORT-FILE-OUTPUT.

           IF ( IO-ALREADY-OPEN )
              CLOSE REPORT-FILE
              GO TO OPEN-REPORT-FILE-OUTPUT.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       OPEN-REPORT-FILE.

           PERFORM OPEN-IT.
           MOVE TR1534-REPORT-IFN TO E-FILE.
           OPEN I-O REPORT-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-REPORT-FILE.

           IF ( IO-ALREADY-OPEN )
              CLOSE REPORT-FILE
              GO TO OPEN-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       READ-REPORT-FILE-NEXT.

           PERFORM READ-IT.
           MOVE TR1534-REPORT-IFN TO E-FILE.
           MOVE TR1534-REPORT-KEY TO E-KEYX.
           READ REPORT-FILE NEXT.

           IF ( IO-LOCKED )
              GO TO READ-REPORT-FILE-NEXT.

      *-----------------------------------------------------------------
       START-REPORT-FILE.

           PERFORM START-IT.
           MOVE TR1534-REPORT-IFN TO E-FILE.
           MOVE TR1534-REPORT-KEY TO E-KEYX.
           START REPORT-FILE KEY NOT < TR1534-REPORT-KEY.
           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       WRITE-REPORT-FILE.

           PERFORM WRITE-IT.
           MOVE TR1534-REPORT-IFN TO E-FILE.
           MOVE TR1534-REPORT-KEY TO E-KEYX.

           WRITE REPORT-REC.

           IF ( IO-LOCKED )
             GO TO WRITE-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       CLOSE-REPORT-FILE.

           PERFORM CLOSE-IT.
           CLOSE REPORT-FILE.

      *=================================================================
       OPEN-MSEXCEL-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE MSEXCEL-PATH TO E-FILE.
           OPEN OUTPUT MSEXCEL-FILE.

           IF ( IO-FG = 8 )
              GO TO OPEN-MSEXCEL-FILE-OUTPUT.

           IF ( IO-FG = 7 )
              CLOSE MSEXCEL-FILE
              GO TO OPEN-MSEXCEL-FILE-OUTPUT.

      *-----------------------------------------------------------------
       READ-MSEXCEL-FILE.
           PERFORM READ-IT.
           MOVE MSEXCEL-PATH TO E-FILE.
           READ MSEXCEL-FILE.

           IF ( IO-FG = 8 )
              GO TO READ-MSEXCEL-FILE.

      *-----------------------------------------------------------------
       WRITE-MSEXCEL-FILE.
           PERFORM WRITE-IT.
           MOVE MSEXCEL-PATH TO E-FILE.
           WRITE MSEXCEL-REC.

           IF ( IO-FG = 8 )
              GO TO WRITE-MSEXCEL-FILE.

      *-----------------------------------------------------------------
       CLOSE-MSEXCEL-FILE.
           PERFORM CLOSE-IT.
           CLOSE MSEXCEL-FILE.

      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.

           PERFORM CLOSE-FILES.
           PERFORM MISC-TRACE-END-PROGRAM.             *> DISABLE TRACE

           MOVE SORT-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF ( TR1534-EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVING REPORT FILE IN /USR/TMP TO /USR/TMP/TR1534.MKTEMP
      *    SAVING REPORT FILE IN CASE OF ANY OF THE UPGRADE PROGRAMS
      *    BOMB OUT SO YOU CAN SEE WHICH FILE WAS PROCESSED LAST.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "MV "                      TO SAVE-REPORT-CMD.
      *    CALL "C$TOLOWER" USING SAVE-REPORT-CMD, VALUE 03.

      *    MOVE TR1534-REPORT-PATH         TO MKTEMP-BUF
      *                                       SAVE-REPORT-SOURCE
      *                                       SAVE-REPORT-TARGET.

      *    MOVE "TR1534."                  TO SAVE-REPORT-TARGET-NEW.
      *    CALL "C$TOLOWER" USING SAVE-REPORT-TARGET-NEW, VALUE 07.

      *    MOVE MKTEMP-NAME                TO SAVE-REPORT-TARGET-MKTEMP.

      *    MOVE SAVE-REPORT-COMMAND        TO SYSTEM-BUF.
      *    PERFORM SYSTEM-CALL.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y" )
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH
                 MOVE SV-GROUP-FLAG        TO GROUP-FLAG
                 MOVE SV-ENT-GROUP         TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH               TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.
           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY TR1534-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.


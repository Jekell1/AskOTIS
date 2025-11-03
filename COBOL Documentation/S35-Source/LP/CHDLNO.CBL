       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CHDLNO.
       DATE-WRITTEN.  021390.
      ******************************************************************
      *
      * IDENTIFICATION:  LP/CHDLNO.C
      *
      * DESCRIPTION   :  CHANGE MASTER DEALER NUMBERS
      *
      *-----------------------------------------------------------------
      *   THIS PROGRAM ALLOW THE ENTRY OF A NEW DEALER NUMBER AND
      *   CHANGES, DELETES AND CREATES ALL RECORDS:
      *
      *     1. READS ORIGINAL AND DELETES DL RECORD
      *     2. TRYS TO CREATE NEW DL RECORD, BUT IF IT EXISTS,
      *        IT WILL REWRITE WITH ORIGIANL FORMULAS
      *     3. THEN CHANGES ALL OTHER FILES NECESSARY
      *
      *-----------------------------------------------------------------
      * REV:
      * MJD 091595 ADDED DS-DATE LOGIC CHANGED HOLD-DS-REC
      *             FROM 1970 TO 2150 BYTES.
      * MJD 112597 EXPANDED DT-REC TO 162 BYTES
      * MJD 052798 CHANGED DS-REC SIZE TO 2023 (USED 78 LEVEL DS-SIZE)
      * GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL
      *             PER DEVELOPMENT MEETING (082599)
      * KEC 000515 {PR#01189} PARADATA FINANCIAL [LORI MCBEE];
      *            ADDED LOGIC TO CHANGE DEALER NUMBERS IN THE
      *            LOAN APPLICATION FILES (BA/LA/MA).
      *
      * KEC 2016.0510 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED MA-APPNO TO USE MA-APP-NUMBER FOR MA1-KEY.
      * BLM 160511 INCREASE HOLD-DS2-KEY, PD0003
      *
      * KEC 2016.0818 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USE OF THE BA-FILE [LIBLA/LAFDMX].
      *         REVISED LA-REC LAYOUT.
      *
      * KEC 2016.1221 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FIELD WORKERS TO CONTAIN FILE PREFIXES (DL).
      *
      * KEC 2017.0103 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * KEC 2018-0823 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 181008 GLOBAL CKFILE
      * BAH 181009 GLOBAL CXFILE
      * BAH 181010 GLOBAL DLFILE
      * BAH 181015 GLOBAL DMFILE
      * BAH 181015 GLOBAL DSFILE
      * BAH 181029 GLOBAL DTFILE
      * BAH 181126 GLOBAL LHFILE
      * BAH 181204 GLOBAL LNFILE
      * BAH 190111 GLOBAL PDFILE
      * BAH 190218 CHANGED SAVE-NEW-DL1-KEY TO JUST SAVE-NEW-DL-DLNO
      *            KEEP DLNO CONSISTENT, DONT USE DLNUM, TRYING TO CLEAN
      *            UP ALL THE REDEFINES FOR SQL
      *  CS 190429 REMOVED APPS, SAVED CHDLNO.190429
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/CHDLNO.CBL S35 02/20/24-13:22:42 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01DM.CPY".
       COPY "LIBLP/LP01DM_SQL.CPY".
       COPY "LIBLP/LPWSDM.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ACTION.
           03  CHKEY                   PIC X(04).
           03  CHNO                    PIC 9(02).

       01  ALREADY-EXISTS-MESS.
           03  FILLER PIC X(30) VALUE "DEALER NUMBER ALREADY EXISTS, ".
           03  FILLER PIC X(30) VALUE "TYPE 'YES' TO CONTINUE..      ".

       01  ELE                         PIC 9(02).
       01  FIRST-SCAN                  PIC 9(01)     VALUE ZEROES.
       01  HOLD-CH-SEQ                 PIC 9(3).
       01  HOLD-DS-REC                 PIC X(DS-SIZE).
      *01  HOLD-DS2-KEY                PIC 9(13).
       01  HOLD-DT-REC                 PIC X(DT-SIZE).
       01  IO-TYPE                     PIC X(03).
       01  NAME-WORKER                 PIC X(34).
       01  NEW-DLNO-EXISTS             PIC X(01)     VALUE SPACES.
       01  NEW-FLAG                    PIC 9         VALUE 1.
       01  ONE                         PIC S9(01).
       01  ORIGINAL-DL-REC             PIC X(DL-SIZE).
           COPY "LIBLP/LP01DM.CPY" REPLACING LEADING "DM" BY "HOLD-DM".
       01  RTNCD                       PIC X(03).


       01  SAVE-NEW-DL-DLNO            PIC 9(4)V99.
       01  SAVE-OLD-DL-DLNO            PIC 9(4)V99.

      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       01  DISPLAY-BUF                               VALUE SPACES.
           03  FDLNO                   PIC ZZZ9.99.
           03  FNAME                   PIC X(32).
           03  FADDR1                  PIC X(30).
           03  FADDR2                  PIC X(30).
           03  FADDR3                  PIC X(30).

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER PIC X(28) VALUE "DLNO NAME ADDR1 ADDR2 ADDR3.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       78  MAX                                       VALUE 1.
      *-----------------------------------------------------------------
       01  SPEC-TABLE.
      *- - - - - - - - - - - - - - - - - - - -{ 01. NEW DEALER NUMBER  }
           03  FILLER PIC X(08) VALUE "ENNN042 ".

      *-----------------------------------------------------------------
       01  SPEC-TAB                    REDEFINES SPEC-TABLE.
           03  SPEC-REC                OCCURS MAX TIMES.
               05  SPEC                PIC X(07).
               05  CHFG                PIC X(01).
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CHDLNO_DEF.CPY".
       COPY "LIBLP/CHDLNO_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

      *-----------------------------------------------------------------
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------

      *=================================================================
      *    L I N K A G E - S E C T I O N
      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/CHDLNO_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-PROGRAM.                                                

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHDLNO"      TO VDU-FORM-NAME.

           MOVE SPACES TO ERRCD.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE ZEROES TO SAVE-OLD-DL-DLNO
                          SAVE-NEW-DL-DLNO.

           PERFORM REC-DL-MODULE.

           IF ( IO-TYPE = "END" ) GO TO MAIN-PROGRAM-EXIT.
           IF ( IO-TYPE = "CLR" ) GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF ( IO-TYPE = "UPD" ) PERFORM UPDATE-MODULE.

           GO TO MAIN-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       REC-DL-MODULE SECTION.
      ******************************************************************
       REC-DL-MODULE-NO.

           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.
           MOVE 0 TO SAVE-OLD-DL-DLNO.

           MOVE "ENTER ORIGINAL DEALER #, F6-SCAN, F7-EXIT" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN042DLNO." TO VDU.
           PERFORM SCREENIO.

           IF (        F6-KEY ) PERFORM ENTER-SCANKEY
                                GO TO END-REC-DL-MODULE.
           IF (        F7-KEY ) MOVE "END" TO IO-TYPE
                                GO TO END-REC-DL-MODULE.
           IF ( NOT ENTER-KEY ) GO TO REC-DL-MODULE-NO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM2 TO SAVE-OLD-DL-DLNO.

           IF ( SAVE-OLD-DL-DLNO = 0 )
              GO TO REC-DL-MODULE-NO.

           MOVE BRANCH           TO DL-BRNO.
           MOVE SAVE-OLD-DL-DLNO TO DL-DLNO.

           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF ( IO-BAD )
              GO TO REC-DL-MODULE-NO.

           MOVE DL-REC TO ORIGINAL-DL-REC.

           PERFORM DISPLAY-FIELDS.

       END-REC-DL-MODULE.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE " ENTER DEALER ABRV-NM, OR F6 TO SCAN" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-DL2-FILE.

           MOVE BRANCH    TO DL-BRNO.
           MOVE SPACES    TO DL-SHORT.

           PERFORM START-DL2-FILE.

      *-----------------------------------------------------------------
       ENTER-KEY.

           MOVE "E RA160SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) MOVE "CLR" TO IO-TYPE
                                GO TO CLOSE-SCANKEY.
           IF (        F6-KEY ) GO TO READ-FILES-NEXT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = SPACES )
              IF ( FIRST-SCAN NOT = ZEROES )
                 MOVE DL-REC TO ORIGINAL-DL-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE DL-PATH-OWNBR TO DL-BRNO.
           MOVE VDUBUF        TO DL-SHORT. 

           PERFORM START-DL2-FILE.

      *=================================================================
       READ-FILES-NEXT.

           PERFORM READ-DL2-FILE-NEXT.
           IF ( IO-BAD ) OR (DL-BRNO NOT = DL-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK DL-FILE.

           MOVE "SNNN042DLNO." TO VDU.
           MOVE DL-DLNO        TO VDUNUM2.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.

      *-----------------------------------------------------------------
       CLOSE-SCANKEY.
           MOVE ZEROES TO FIRST-SCAN.
           PERFORM CLOSE-DL2-FILE.

           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.

      *=================================================================
       END-SCANKEY.
           EXIT.
      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *-----------------------------------------------------------------
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO ONE.
           MOVE ZEROES TO ELE.

      *=================================================================
       CREATE-ROUTINE.

           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE     < 1   ) MOVE 1 TO ELE ONE.
           IF ( ELE NOT > MAX ) GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.

           MOVE "ENTER FIELD #, F1-CANCEL, <ENTER>-ACCEPT" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (        F1-KEY ) MOVE "CLR" TO IO-TYPE
                                GO TO END-ENTRY-MODULE.
           IF ( NOT ENTER-KEY ) GO TO MODIFY-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              IF ( SAVE-NEW-DL-DLNO = 0 )
                 MOVE "CLR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE
              ELSE
                 MOVE "UPD" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROES.

           IF ( CHNO NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( CHNO = ZEROES    ) GO TO MODIFY-ROUTINE.
           IF ( CHNO > MAX       ) GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.

           IF ( ELE = 1 )
              MOVE "OLDNAME OLDACCEPT." TO MESS
              MOVE SPACES TO WR-BUF
              MOVE MESS   TO WR-LIST
              PERFORM CALL-WRFORM.

           IF ( CHKEY    NOT = ZEROES ) AND
              ( CHFG (ELE)   = ZEROES ) AND
              ( NEW-FLAG NOT = 1      )
              GO TO MODIFY-ROUTINE.

           IF ( CHFG (ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO CREATE-ROUTINE
              ELSE
              IF ( CHKEY NOT = EXT-CONAME-DAILY-PASSWORD )
                 GO TO MODIFY-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - -{ ENTER ON ELEMENT }
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE ZEROES    TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ TEST KEYS  }
           IF ( F1-KEY )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( DOWN-KEY )
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

           IF ( UP-KEY )
              IF ( NEW-FLAG = 1 )
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

           IF ( NOT ENTER-KEY )
              GO TO ENTER-ELE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.
           GO TO F-01.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.

           MOVE VDUNUM2 TO SAVE-NEW-DL-DLNO.

           IF ( SAVE-NEW-DL-DLNO = 0 )
              GO TO ENTER-ELE.

           MOVE BRANCH           TO DL-BRNO.
           MOVE SAVE-NEW-DL-DLNO TO DL-DLNO.

           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.

           MOVE SPACES TO NEW-DLNO-EXISTS.

           IF ( IO-GOOD )
              MOVE "S  A320OLDNAME." TO VDU
              MOVE DL-NAME           TO VDUBUF
              PERFORM SCREENIO
              MOVE "DEALER ALREADY EXISTS !!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       UPDATE-MODULE SECTION.
      ******************************************************************

           PERFORM UPDATE-DELETE-OLD-DL.

           IF ( RTNCD = "END" )
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-DL1-FILE.

           IF ( RTNCD = "END" )
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-DM1-FILE.

           IF ( RTNCD = "END" )
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-LN1-FILE.

           IF ( RTNCD = "END" )
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-LH1-FILE.

           IF ( RTNCD = "END" )
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-CX1-FILE.

           IF ( RTNCD = "END" )
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-CK1-FILE.

           IF ( RTNCD = "END" )
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-PD1-FILE.

           IF ( RTNCD = "END" )
              GO TO END-UPDATE-MODULE.

      D    STOP MESS.
           PERFORM UPDATE-DS2-FILE.

           IF ( RTNCD = "END" )
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-DT1-FILE.

           IF ( RTNCD = "END" )
              GO TO END-UPDATE-MODULE.

           PERFORM CREATE-CHANGE-REC.

       END-UPDATE-MODULE.
           EXIT.

      ******************************************************************
       UPDATE-DELETE-OLD-DL SECTION.
      ******************************************************************

           MOVE SPACES TO RTNCD.

           MOVE "DELETING OLD DEALER RECORD!" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE ORIGINAL-DL-REC TO DL-REC.
           MOVE DL-DLNO         TO SAVE-OLD-DL-DLNO.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           IF ( IO-BAD )
              PERFORM RECORD-DELETED
              MOVE "END" TO RTNCD
              GO TO UPDATE-DELETE-OLD-DL-END.

           IF ( DL-REC NOT = ORIGINAL-DL-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              MOVE "END" TO RTNCD
              GO TO UPDATE-DELETE-OLD-DL-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ UPDATE }
           PERFORM DELETE-DL1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

       UPDATE-DELETE-OLD-DL-END.

           PERFORM CLOSE-DL1-FILE.

       UPDATE-DELETE-OLD-DL-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-DL1-FILE SECTION.
      ******************************************************************

      * CREATE DEALER RECORD WITH NEW DLNO:

           MOVE SPACES TO RTNCD.

           MOVE "CREATING NEW DEALER RECORD!" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE ORIGINAL-DL-REC  TO DL-REC.
           MOVE SAVE-NEW-DL-DLNO TO DL-DLNO.
           PERFORM OPEN-DL1-FILE.
           PERFORM WRITE-DL1-FILE.
           IF ( IO-BAD )
              PERFORM REWRITE-DL1-FILE.

           PERFORM CLOSE-DL1-FILE.

       UPDATE-DL1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *    DELETE ALL DEALER MEMO RECORDS WITH THE OLD KEY AND
      *    RE-CREATE WITH NEW KEY:
      ******************************************************************
       UPDATE-DM1-FILE SECTION.

           MOVE SPACES TO RTNCD.

           MOVE "UPDATING ALL MEMO RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-DM1-FILE.

           MOVE DM-PATH-OWNBR    TO DM-BRNO
                                    QDM1-WBEG-BRNO
                                    QDM1-WEND-BRNO.
           MOVE SAVE-OLD-DL-DLNO TO DM-DL-DLNO
                                    QDM1-WBEG-DLNO
                                    QDM1-WEND-DLNO.
           MOVE 0                TO DM-DATE DM-SEQ
                                    QDM1-WBEG-DATE-REV
                                    QDM1-WBEG-SEQ.

           MOVE ALL "9"          TO QDM1-WEND-DATE-REV
                                    QDM1-WEND-SEQ.
           PERFORM START-DM1-FILE.

           IF ( IO-BAD ) 
              GO TO UPDATE-DM1-FILE-END.

       UPDATE-DM1-NEXT.

           PERFORM READ-DM1-FILE-NEXT.
           IF ( IO-BAD ) OR (DM-BRNO NOT = DM-PATH-OWNBR)
              GO TO UPDATE-DM1-FILE-END.

           IF ( DM-DL-DLNO NOT = SAVE-OLD-DL-DLNO )
              GO TO UPDATE-DM1-FILE-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DM-REC          TO HOLD-DM-REC.
           PERFORM DELETE-DM1-FILE.

           MOVE HOLD-DM-REC      TO DM-REC.
           MOVE SAVE-NEW-DL-DLNO TO DM-DL-DLNO.
           PERFORM WRITE-DM1-FILE.

           GO TO UPDATE-DM1-NEXT.

       UPDATE-DM1-FILE-END.

           PERFORM CLOSE-DM1-FILE.

       UPDATE-DM1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *    CHANGE ALL LOAN RECORDS
      ******************************************************************
       UPDATE-LN1-FILE SECTION.

           MOVE SPACES TO RTNCD.

           MOVE "UPDATING ALL LOAN RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.
           IF ( IO-BAD )
              GO TO UPDATE-LN1-FILE-END.

       UPDATE-LN1-NEXT.

           PERFORM READ-LN1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LN-OWNBR NOT = LN-PATH-OWNBR )
              GO TO UPDATE-LN1-FILE-END.

           IF ( LN-DLNO NOT = SAVE-OLD-DL-DLNO )
      *       UNLOCK LN-FILE
              GO TO UPDATE-LN1-NEXT.

           MOVE SAVE-NEW-DL-DLNO TO LN-DLNO.
           PERFORM REWRITE-LN1-FILE.

           GO TO UPDATE-LN1-NEXT.

       UPDATE-LN1-FILE-END.

           PERFORM CLOSE-LN1-FILE.

       UPDATE-LN1-FILE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-LH1-FILE SECTION.
      ******************************************************************

           MOVE SPACES TO RTNCD.

           MOVE "UPDATING ALL LOAN HISTORY RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LH1-FILE.
           MOVE LH-PATH-OWNBR TO LH-BRNO.
           MOVE 0             TO LH-SSNO LH-ACCTNO.

           PERFORM START-LH1-FILE.
           IF ( IO-BAD ) 
              GO TO UPDATE-LH1-FILE-END.

      *=================================================================
       UPDATE-LH1-NEXT.
           PERFORM READ-LH1-FILE-NEXT.
           IF ( IO-BAD ) OR (LH-BRNO NOT = LH-PATH-OWNBR)
              GO TO UPDATE-LH1-FILE-END.

           IF ( LH-DLNO NOT = SAVE-OLD-DL-DLNO )
      *       UNLOCK LH-FILE
              GO TO UPDATE-LH1-NEXT.

           MOVE SAVE-NEW-DL-DLNO TO LH-DLNO.
           PERFORM REWRITE-LH1-FILE.

           GO TO UPDATE-LH1-NEXT.

       UPDATE-LH1-FILE-END.
           PERFORM CLOSE-LH1-FILE.

       UPDATE-LH1-FILE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-CX1-FILE SECTION.
      ******************************************************************

           MOVE SPACES TO RTNCD.

           MOVE "UPDATING ALL TEMP CHECK RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-CX1-FILE.

           MOVE SPACES        TO CX1-KEY.
           MOVE CX-PATH-OWNBR TO CX-BRNO.
           MOVE 0             TO CX-SEQNO.

           PERFORM START-CX1-FILE.
           IF ( IO-BAD )
              GO TO UPDATE-CX1-FILE-END.

       UPDATE-CX1-NEXT.

           PERFORM READ-CX1-FILE-NEXT.
           IF ( IO-BAD ) OR (CX-BRNO NOT = CX-PATH-OWNBR)
              GO TO UPDATE-CX1-FILE-END.

           IF ( CX-DLNO NOT = SAVE-OLD-DL-DLNO )
      *       UNLOCK CX-FILE
              GO TO UPDATE-CX1-NEXT.

           MOVE SAVE-NEW-DL-DLNO TO CX-DLNO.
           PERFORM REWRITE-CX1-FILE.
           GO TO UPDATE-CX1-NEXT.

       UPDATE-CX1-FILE-END.

           PERFORM CLOSE-CX1-FILE.

       UPDATE-CX1-FILE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-CK1-FILE SECTION.
      ******************************************************************

           MOVE SPACES TO RTNCD.

           MOVE "UPDATING ALL CHECK RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-CK1-FILE.

           MOVE CX-PATH-OWNBR TO CK-BRNO.
           MOVE 0             TO CK-NO.

           PERFORM START-CK1-FILE.
           IF ( IO-BAD )
              GO TO UPDATE-CK1-FILE-END.

       UPDATE-CK1-NEXT.

           PERFORM READ-CK1-FILE-NEXT.
           IF ( IO-BAD ) OR ( CK-BRNO NOT = CK-PATH-OWNBR )
              GO TO UPDATE-CK1-FILE-END.

           IF ( CK-DLNO NOT = SAVE-OLD-DL-DLNO )
      *       UNLOCK CK-FILE
              GO TO UPDATE-CK1-NEXT.

           MOVE SAVE-NEW-DL-DLNO TO CK-DLNO.
           PERFORM REWRITE-CK1-FILE.

           GO TO UPDATE-CK1-NEXT.

       UPDATE-CK1-FILE-END.

           PERFORM CLOSE-CK1-FILE.

       UPDATE-CK1-FILE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-DS2-FILE SECTION.
      ******************************************************************

           MOVE SPACES TO RTNCD.

           MOVE "UPDATING DEALER STATISTIC RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-DS2-FILE.

           MOVE BRANCH           TO DS-BRANCH
                                    QDS2-WBEG-BRANCH
                                    QDS2-WEND-BRANCH.
           MOVE SAVE-OLD-DL-DLNO TO DS-DLNO
           MOVE DS-NUM           TO QDS2-WBEG-NUM
                                    QDS2-WEND-NUM.
           MOVE ZEROES           TO DS-CLASS
                                    QDS2-WBEG-CLASS.
           MOVE ALL "9"          TO QDS2-WEND-CLASS.

           PERFORM START-DS2-FILE.
           IF ( IO-BAD )
              GO TO UPDATE-DS2-FILE-END.

       UPDATE-DS2-NEXT.

           PERFORM READ-DS2-FILE-NEXT.
           IF ( IO-BAD ) OR (DS-BRANCH NOT = DS-PATH-OWNBR)
              GO TO UPDATE-DS2-FILE-END. 

           IF ( DS-DLNO > SAVE-OLD-DL-DLNO )
              GO TO UPDATE-DS2-FILE-END.

      *    MOVE DS2-KEY TO HOLD-DS2-KEY.

           MOVE DS-REC TO HOLD-DS-REC.
           PERFORM READ-DS1-FILE.
           PERFORM DELETE-DS1-FILE.

           MOVE HOLD-DS-REC      TO DS-REC.
           MOVE SAVE-NEW-DL-DLNO TO DS-DLNO.
           PERFORM WRITE-DS1-FILE.

      *    MOVE HOLD-DS2-KEY TO DS2-KEY.

      *    PERFORM START-DS2-FILE.

           GO TO UPDATE-DS2-NEXT.

       UPDATE-DS2-FILE-END.

           PERFORM CLOSE-DS1-FILE.

       UPDATE-DS2-FILE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-DT1-FILE SECTION.
      ******************************************************************

           MOVE SPACES TO RTNCD.

           MOVE "UPDATING DEALER TRANSACTION RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-DT1-FILE.

           MOVE BRANCH            TO DT-BRNO
                                     QDT1-WBEG-BRNO
                                     QDT1-WEND-BRNO.
           MOVE SAVE-OLD-DL-DLNO  TO DT-DLNO.
           MOVE DT-NUM            TO QDT1-WBEG-NUM
                                     QDT1-WEND-NUM.

           MOVE 0          TO DT-CLASS DT-ACCTNO DT-SEQ
                              QDT1-WBEG-CLASS
                              QDT1-WBEG-ACCTNO
                              QDT1-WBEG-SEQ.

           MOVE ALL "9"    TO QDT1-WEND-CLASS
                              QDT1-WEND-ACCTNO
                              QDT1-WEND-SEQ.
 
           MOVE 19000101   TO DT-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WBEG-DATE.
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WEND-DATE

           PERFORM START-DT1-FILE.
           IF ( IO-BAD )
              GO TO UPDATE-DT1-FILE-END.

      *=================================================================
       UPDATE-DT1-NEXT.

           PERFORM READ-DT1-FILE-NEXT.
           IF ( IO-BAD ) OR (DT-BRNO NOT = DT-PATH-OWNBR)  
              GO TO UPDATE-DT1-FILE-END.

           IF ( DT-DLNO NOT = SAVE-OLD-DL-DLNO )
      *       UNLOCK DT-FILE
              GO TO UPDATE-DT1-NEXT.

           MOVE DT-REC      TO HOLD-DT-REC.
           PERFORM DELETE-DT1-FILE.
           MOVE HOLD-DT-REC TO DT-REC.
           MOVE SAVE-NEW-DL-DLNO TO DT-DLNO.
           PERFORM WRITE-DT1-FILE.

           GO TO UPDATE-DT1-NEXT.

       UPDATE-DT1-FILE-END.

           PERFORM CLOSE-DT1-FILE.

       UPDATE-DT1-FILE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-PD1-FILE SECTION.
      ******************************************************************

           MOVE SPACES TO RTNCD.

           MOVE "UPDATING ALL SCRATCH RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-PD1-FILE.

           MOVE "L"           TO PD-ID
                                 QPD1-WBEG-ID
                                 QPD1-WEND-ID.
           MOVE PD-PATH-OWNBR TO PD-BRANCH
                                 QPD1-WBEG-BRANCH
                                 QPD1-WEND-BRANCH.
           MOVE ZEROES        TO PD-SOC-SEC
                                 QPD1-WBEG-SOC-SEC.
           MOVE ALL "9"       TO QPD1-WEND-SOC-SEC.

           PERFORM START-PD1-FILE.

       UPDATE-PD1-NEXT.
           PERFORM READ-PD1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO UPDATE-PD1-FILE-END.

           IF ( PD-ID NOT = "L" ) OR ( PD-BRANCH NOT = PD-PATH-OWNBR )
              GO TO UPDATE-PD1-FILE-END.

           IF ( PD-DLNO NOT = SAVE-OLD-DL-DLNO )
      *       UNLOCK CK-FILE
              GO TO UPDATE-PD1-NEXT.

           MOVE SAVE-NEW-DL-DLNO TO PD-DLNO.
           PERFORM REWRITE-PD1-FILE.

           GO TO UPDATE-PD1-NEXT.

       UPDATE-PD1-FILE-END.

           PERFORM CLOSE-PD1-FILE.

       UPDATE-PD1-FILE-EXIT.
           EXIT.


      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES TO DISPLAY-BUF.

           MOVE DL-DLNO TO FDLNO.
           MOVE DL-NAME TO FNAME.
           MOVE DL-ADR1 TO FADDR1.
           PERFORM SETUP-CITY-STATE-ZIP.

           IF ( DL-ADR2 = SPACES )
              MOVE NAME-WORKER TO FADDR2
           ELSE
              MOVE DL-ADR2     TO FADDR2
              MOVE NAME-WORKER TO FADDR3.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      *********************************************
       CREATE-CHANGE-REC SECTION.
      *********************************************
           PERFORM GETDAT.

           PERFORM OPEN-CH-FILE.
           MOVE 1               TO HOLD-CH-SEQ.
       CREATE-CHANGE-REC-AGAIN.
           PERFORM CLEAR-CH-FILE.

           MOVE BRANCH             TO CH-BRNO.
           MOVE SYS-DATE           TO CH-DATE.
           MOVE "A"                TO CH-CODE.
           MOVE EXT-CONAME-USER-ID TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS           TO CH-TIME.
           MOVE HOLD-CH-SEQ        TO CH-SEQ.

           MOVE "CHANGE DEALER NO." TO CH-DESCRIPT.
           MOVE SAVE-OLD-DL-DLNO    TO CH-ORG-2DEC.
           MOVE SAVE-NEW-DL-DLNO    TO CH-NEW-2DEC.

           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF HOLD-CH-SEQ < 999
                 ADD 1 TO HOLD-CH-SEQ
                 GO TO CREATE-CHANGE-REC-AGAIN.

           PERFORM CLOSE-CH-FILE.
       EXIT-CREATE-CHANGE-REC.
           EXIT.

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE LPLOAN-CONAME TO CHDLNO-CONAME.
           MOVE LPLOAN-SYSDATE TO CHDLNO-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CHDLNO-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE CHDLNO-FORM-FIELDS TO WR-BUF.
           MOVE CHDLNO-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE CHDLNO-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHDLNO-HELP--DIR TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CHDLNO_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPCHCL.CPY".

      *********************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      *********************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETDAT.CPY".
      *-----------------------------------------------------------------
       SEND-LEGEND.

           MOVE "S  A000MESS." TO VDU.
           MOVE MESS           TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       SETUP-CITY-STATE-ZIP.

           MOVE DL-CITY TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ",? ".
           INSPECT NAME-WORKER REPLACING FIRST "  " BY
                                            DL-STATE.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY "?  ".
           INSPECT NAME-WORKER REPLACING FIRST "          "
                                            BY DL-ZIP.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".

      *=================================================================
       CHANGE-AGAIN.

           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-DELETED.

           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.

           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LH1-FILE.
           PERFORM CLOSE-DT1-FILE.
           PERFORM CLOSE-DS2-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DM1-FILE.
           PERFORM CLOSE-DL2-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CH-FILE.
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDMGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBLP/LPCK1IN.CPY".
       COPY "LIBLP/LPCX1IN.CPY".
       COPY "LIBLP/LPDL1IN.CPY".
       COPY "LIBLP/LPDL2RN.CPY".
       COPY "LIBLP/LPDM1IN.CPY".
       COPY "LIBLP/LPDS1IN.CPY".
       COPY "LIBLP/LPDS2RN.CPY".
       COPY "LIBLP/LPDT1IN.CPY".
       COPY "LIBLP/LPLH1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPPD1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHDLNO-SCREEN.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.

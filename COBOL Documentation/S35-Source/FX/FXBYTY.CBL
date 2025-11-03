       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXBYTY.
       DATE-WRITTEN.    060299.
      *************************************************************************
      *
      *  DESCRIPTION:  UPDATE BY FROM TYFILE FOR SELECTED DATE:
      *                WILL READ TY FOR SELECTED BRANCH, CLASS &
      *                DATE & UPDATE BYFILE.  IF TY-BRFG IS ALREADY
      *                "Y", THIS FIX WILL SKIP UNLESS YOU ANSWER
      *                "Y" TO RE-UPDATE QUESTION.
      *
      *=================================================================
      * REV:
      * BLV 050500 UPDATE FOR NEW FIELDS - UNEARNED & PROCEEDS, COMPARE 
      *            & RECONCILE WITH LPEOD5, NOW UP-TO-DATE
      * BLV 050500 FIX TO CHECK CLASS RANGE
      * BLV 021226 ADDED UPDATE OF BY-LOCKBOX
      * BLM 160224 FIX LOGIC FOR NEW BY & TY DATE FORMATS
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      * 180305 BLM ADD UPDATE OF TY-TYPE TABLE FOR NONFILE, AD&D AND
      *            ROADGARD
      * BAH 181017 GLOBAL TYFILE
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      * BAH 190918 REMOVED ACCESS-CALL ON TYFILE
      * BAH 2022.1010 HARD CODED STARTS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.


      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/FXBYTY.CBL S34 06/16/21-11:05:04 >".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       01  SUB               PIC 99.
       01  SUB2              PIC 9(6).
       01  SUB3              PIC 9(6).
       01  ACT               PIC 9(6).
       01  TYP               PIC 9(6).

       01  ERRCD             PIC X     VALUE " ".

       01  ACCEPT-FG         PIC X(7)  VALUE " ".

       01  MESS-LIST         PIC X(5)  VALUE "MESS.".


       01  TODAYS-DATE          PIC 9(8)      VALUE 0.

      *-----------------------------------------------------------------
       01  BEG-BRANCH        PIC 9(4)      VALUE 0.
       01  END-BRANCH        PIC 9(4)      VALUE 0.
       01  BEG-BR               PIC 9999      VALUE 0.
       01  END-BR               PIC 9999      VALUE 0.
       01  BEG-CLASS         PIC 9(3)        VALUE 0.
       01  END-CLASS         PIC 9(3)        VALUE 0.
       01  UPDATE-FG         PIC X         VALUE " ".
       01  SUBTRACT-FG       PIC X         VALUE " ".
       01  GROUP-FLAG        PIC X.
       01  ENT-GROUP         PIC X(4).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPBYCLW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       01  FXBYTY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FXBYTY_DEF.CPY".
       COPY "LIBFX/FXBYTY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/FXBYTY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M - C O N T R O L
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE "FXBYTY" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FXBYTY-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM INITIAL-ROUTINE.
           IF ( ERRCD NOT = " " )
              GO TO END-ROUTINE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           PERFORM GET-GPENV.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.
           IF ( GROUP-FLAG NOT = "Y" )
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = 0 )
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = 0 )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-FG = 9 ) OR ( BR-NO > END-BR )
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BY-PATH-OVERRIDE
                                              TY-PATH-OVERRIDE.

           MOVE " " TO MESS.
           STRING "FIX IN PROGRESS..." BR-NO
                                      DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-LEGEND.

           PERFORM UPDATE-BYFILE.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIAL-ROUTINE SECTION.
      ******************************************************************
           MOVE " " TO ERRCD.

       INIT-ACCEPT-TODAY.
           MOVE "ERNM060TODAYS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-TODAY.

           MOVE VDU-DATE-YYYYMMDD TO TODAYS-DATE.
           IF TODAYS-DATE = 0
              GO TO INIT-ACCEPT-TODAY.

       INIT-ACCEPT-BR1.
           MOVE "   A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
              GO TO INITIALIZE-ENDIT.

           IF ( VDUSTATUS = "16" )
              GO TO INIT-ACCEPT-BR-ALL.

           IF ( VDUSTATUS = "1U" )
              GO TO INIT-ACCEPT-TODAY.

           IF ( VDUSTATUS = "1<" )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO INIT-ACCEPT-BR1
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-BR1.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > 0 )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO INIT-ACCEPT-BR1.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BR BEG-BRANCH
              MOVE 9999 TO END-BR END-BRANCH
              GO TO INIT-ACCEPT-CL1.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.

       INIT-ACCEPT-BR2.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
              GO TO INITIALIZE-ENDIT.

           IF ( VDUSTATUS = "1U" )
              GO TO INIT-ACCEPT-BR1.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-BR2.

           MOVE VDUNUM0 TO END-BR END-BRANCH.

           IF ( BEG-BR > END-BR )
              PERFORM INVALID-RANGE
              GO TO INIT-ACCEPT-BR1.

           GO TO INIT-ACCEPT-CL1.

       INIT-ACCEPT-BR-ALL.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       INIT-ACCEPT-CL1.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.

           IF VDUSTATUS = "16"
              GO TO INIT-ACCEPT-CL-ALL.

           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-BR2.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-CL1.

           MOVE VDUNUM0 TO BEG-CLASS.

       INIT-ACCEPT-CL2.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.

           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-CL1.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-CL2.

           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO INIT-ACCEPT-CL1.

           GO TO INIT-UPDATE-FG.

       INIT-ACCEPT-CL-ALL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       INIT-UPDATE-FG.
           MOVE "E  A010UPDATEFG." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.

           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-CL2.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-UPDATE-FG.

           MOVE VDUBUF TO UPDATE-FG.

           IF NOT (UPDATE-FG = "Y" OR "N")
              GO TO INIT-UPDATE-FG.

       INIT-SUBTRACT-FG.
           MOVE "E  A010SUBTRACTFG." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.

           IF VDUSTATUS = "1U"
              GO TO INIT-UPDATE-FG.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-SUBTRACT-FG.

           MOVE VDUBUF TO SUBTRACT-FG.

           IF NOT (SUBTRACT-FG = "Y" OR "N")
              GO TO INIT-SUBTRACT-FG.

      *-----------------------------------------------------------------
      *    Y - RUN TO COMPLETION
      *    N - CONVERT OPTION
      *-----------------------------------------------------------------
       INIT-ACCEPT-ACCEPT.
           MOVE "S  A040MESS2." TO VDU.
           PERFORM SCREENIO.
           MOVE "E  A070ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.

           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-CL2.

           IF VDUSTATUS NOT = "00"
              GO TO INIT-ACCEPT-ACCEPT.

           MOVE VDUBUF TO ACCEPT-FG.

           IF ACCEPT-FG = "N"
              GO TO INITIALIZE-ENDIT.

           MOVE "PROGRAM IN PROGRESS..........." TO MESS.
           PERFORM SEND-LEGEND.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ***********************************************
       UPDATE-BYFILE SECTION.
      ***********************************************

           PERFORM OPEN-BY1-FILE.
           PERFORM OPEN-TY1-FILE.

           MOVE BR-NO       TO TY-BRNO
                               QTY1-WBEG-BRNO
                               QTY1-WEND-BRNO.
           MOVE TODAYS-DATE TO TY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTY1-WBEG-DATE
                                       QTY1-WEND-DATE.
           MOVE BEG-CLASS   TO TY-CLASS
                               QTY1-WBEG-CLASS.
           MOVE END-CLASS   TO QTY1-WEND-CLASS.

           PERFORM START-TY1-FILE.
       NEXT-TY-TO-BY.
           PERFORM READ-TY1-FILE-NEXT.
           IF IO-FG = 9 OR (TY-BRNO NOT = BR-NO)      
              GO TO UPDATE-BYFILE-EXIT.

           IF TY-CLASS > END-CLASS
              GO TO UPDATE-BYFILE-EXIT.

           IF TY-DATE NOT = TODAYS-DATE
              GO TO UPDATE-BYFILE-EXIT.

           IF UPDATE-FG = "N"
              IF TY-BRFG = "Y"
      *          UNLOCK TY-FILE
                 GO TO NEXT-TY-TO-BY.

           IF TY-CLASS = 0
              GO TO UPDATE-BY-END.


           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM.
           MOVE TY-BRNO     TO BY-BRNO.
           MOVE TY-CLASS    TO BY-CLASS.
           PERFORM READ-BY1-FILE.
           IF IO-FG = 9
              PERFORM CREATE-BY1-REC
              IF IO-FG = 9
                 GO TO UPDATE-BYFILE-EXIT.

           IF SUBTRACT-FG = "Y"
              PERFORM SUBTRACT-BY1-FILE
           ELSE
              PERFORM ADD-BY1-FILE.

           PERFORM REWRITE-BY1-FILE.

       UPDATE-BY-END.
           MOVE "Y" TO TY-BRFG.
           PERFORM REWRITE-TY1-FILE.

           GO TO NEXT-TY-TO-BY.

       UPDATE-BYFILE-EXIT.
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-BY1-FILE.

      ***********************************************
       ADD-BY1-FILE SECTION.
      ***********************************************
           ADD TY-DLPARVHELD(1) TO BY-DLPARVHELD(1 1).
           ADD TY-DLPARVHELD(2) TO BY-DLPARVHELD(1 2)
                                   BY-DLPARVHELD(2 2).
           ADD TY-DLDISC      TO BY-DLDISC(1) BY-DLDISC(2).
           ADD TY-AMT-SOLD    TO BY-AMT-SOLD(1) BY-AMT-SOLD(2).
           ADD TY-AMT-TRANS   TO BY-AMT-TRANS(1) BY-AMT-TRANS(2).
           ADD TY-AMT-PURCH   TO BY-AMT-PURCH(1) BY-AMT-PURCH(2).
           ADD TY-NO-SOLD  TO BY-NO-SOLD(1) BY-NO-SOLD(2).
           ADD TY-NO-TRANS TO BY-NO-TRANS(1) BY-NO-TRANS(2).
           ADD TY-NO-PURCH TO BY-NO-PURCH(1) BY-NO-PURCH(2).
           ADD TY-OTHCOL   TO BY-OTHCOL(1) BY-OTHCOL(2).
           ADD TY-OT2COL   TO BY-OT2COL(1) BY-OT2COL(2).
           ADD TY-INTCOL   TO BY-INTCOL(1) BY-INTCOL(2).
           ADD TY-LATECOL  TO BY-LATECOL(1) BY-LATECOL(2).
           ADD TY-PRINCOL  TO BY-PRINCOL(1) BY-PRINCOL(2).
           ADD TY-DEFCOL   TO BY-DEFCOL(1) BY-DEFCOL(2).
           ADD TY-RNPRIN   TO BY-RNPRIN(1) BY-RNPRIN(2).
           ADD TY-INTREB   TO BY-INTREB(1) BY-INTREB(2).
           ADD TY-CLREB1   TO BY-CLREB1(1) BY-CLREB1(2).
           ADD TY-CLREB2   TO BY-CLREB2(1) BY-CLREB2(2).
           ADD TY-CLREB3   TO BY-CLREB3(1) BY-CLREB3(2).
           ADD TY-CLREB4   TO BY-CLREB4(1) BY-CLREB4(2).
           ADD TY-AHREB    TO BY-AHREB(1) BY-AHREB(2).
           ADD TY-PPREB    TO BY-PPREB(1) BY-PPREB(2).
           ADD TY-O1REB    TO BY-O1REB(1) BY-O1REB(2).
           ADD TY-O2REB    TO BY-O2REB(1) BY-O2REB(2).
           ADD TY-O3REB    TO BY-O3REB(1) BY-O3REB(2).
           ADD TY-O4REB    TO BY-O4REB(1) BY-O4REB(2).
           ADD TY-O5REB    TO BY-O5REB(1) BY-O5REB(2).
           ADD TY-O6REB    TO BY-O6REB(1) BY-O6REB(2).
           ADD TY-O7REB    TO BY-O7REB(1) BY-O7REB(2).
           ADD TY-SCREB    TO BY-SCREB(1) BY-SCREB(2).
           ADD TY-MFREB    TO BY-MFREB(1) BY-MFREB(2).
           ADD TY-DFREB    TO BY-DFREB(1) BY-DFREB(2).
           ADD TY-PART-CHGOFF TO BY-PART-CHGOFF(1)
                                 BY-PART-CHGOFF(2).
           ADD TY-ALLOTMENTS TO BY-ALLOTMENTS(1)
                                BY-ALLOTMENTS(2).
           ADD TY-LOCKBOX    TO BY-LOCKBOX(1)
                                BY-LOCKBOX(2).
           ADD TY-SUMWGTTM TO BY-SUMWGTTM.
           ADD TY-SUMWGTAV TO BY-SUMWGTAV.
           ADD TY-SUMWGTTA TO BY-SUMWGTTA.

           ADD TY-OTH-NO-COL TO BY-OTH-NO-COL(1)
                                BY-OTH-NO-COL(2).
           ADD TY-OT2-NO-COL TO BY-OT2-NO-COL(1)
                                BY-OT2-NO-COL(2).
           ADD TY-INT-NO-COL TO BY-INT-NO-COL(1)
                                BY-INT-NO-COL(2).
           ADD TY-LATE-NO-COL TO BY-LATE-NO-COL(1)
                                 BY-LATE-NO-COL(2).

           ADD TY-OTH-NO-WAIVED TO BY-OTH-NO-WAIVED(1)
                                   BY-OTH-NO-WAIVED(2).
           ADD TY-OT2-NO-WAIVED TO BY-OT2-NO-WAIVED(1)
                                   BY-OT2-NO-WAIVED(2).
           ADD TY-INT-NO-WAIVED TO BY-INT-NO-WAIVED(1)
                                   BY-INT-NO-WAIVED(2).
           ADD TY-LATE-NO-WAIVED TO BY-LATE-NO-WAIVED(1)
                                    BY-LATE-NO-WAIVED(2).

           ADD TY-OTH-AMT-WAIVED TO BY-OTH-AMT-WAIVED(1)
                                    BY-OTH-AMT-WAIVED(2).
           ADD TY-OT2-AMT-WAIVED TO BY-OT2-AMT-WAIVED(1)
                                    BY-OT2-AMT-WAIVED(2).
           ADD TY-INT-AMT-WAIVED TO BY-INT-AMT-WAIVED(1)
                                    BY-INT-AMT-WAIVED(2).
           ADD TY-LATE-AMT-WAIVED TO BY-LATE-AMT-WAIVED(1)
                                     BY-LATE-AMT-WAIVED(2).

           IF GP-UNEARNED-STATS = "Y"
              ADD TY-PROCEEDS TO BY-PROCEEDS(1)
                                 BY-PROCEEDS(2).

           PERFORM UPDATE-BY-PL-RECOVERED VARYING
              SUB FROM 1 BY 1 UNTIL SUB > 5.

           PERFORM UPDATE-BY-INS-TYPE VARYING
              SUB FROM 1 BY 1 UNTIL SUB > 10.

           PERFORM UPDATE-BY-FEE-TYPE VARYING
              SUB FROM 1 BY 1 UNTIL SUB > 3.

           PERFORM UPDATE-BY-ACTIVITY VARYING
              ACT FROM 1 BY 1 UNTIL ACT > 2.

           PERFORM UPDATE-BY-DEL
              VARYING SUB3 FROM 1 BY 1 UNTIL SUB3 > 10
                AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    UPDATE UNEARNED STATISTICS NET AND VOLUME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GP-UNEARNED-STATS = "Y"
              PERFORM VARYING BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 6
                        AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                  IF BYSUB2 = 1
                     ADD TY-UNEARNED-AMT(BYSUB2 BYSUB3)
                              TO BY-UNEARNED-AMT(1 BYSUB2 BYSUB3)
                  ELSE
                     ADD TY-UNEARNED-AMT(BYSUB2 BYSUB3)
                              TO BY-UNEARNED-AMT(1 BYSUB2 BYSUB3)
                                 BY-UNEARNED-AMT(2 BYSUB2 BYSUB3)
                  END-IF
              END-PERFORM.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           GO TO EXIT-ADD-BY-FILE.

       UPDATE-BY-PL-RECOVERED.
           ADD TY-PL-PRINCOL(SUB) TO BY-PL-PRINCOL(1 SUB)
                                     BY-PL-PRINCOL(2 SUB).
           ADD TY-PL-INTCOL(SUB) TO BY-PL-INTCOL(1 SUB)
                                     BY-PL-INTCOL(2 SUB).
           ADD TY-PL-LATECOL(SUB) TO BY-PL-LATECOL(1 SUB)
                                     BY-PL-LATECOL(2 SUB).
           ADD TY-PL-O2COL(SUB) TO BY-PL-O2COL(1 SUB)
                                     BY-PL-O2COL(2 SUB).

       UPDATE-BY-INS-TYPE.
           ADD TY-INS-NO-ELIG(SUB) TO BY-INS-NO-ELIG(1 SUB)
                                      BY-INS-NO-ELIG(2 SUB).
           ADD TY-INS-NO-WROTE(SUB) TO BY-INS-NO-WROTE(1 SUB)
                                       BY-INS-NO-WROTE(2 SUB).
           ADD TY-INS-NO-NOTWROTE(SUB) TO BY-INS-NO-NOTWROTE(1 SUB)
                                          BY-INS-NO-NOTWROTE(2 SUB).
           ADD TY-INS-AMT-WROTE(SUB) TO BY-INS-AMT-WROTE(1 SUB)
                                        BY-INS-AMT-WROTE(2 SUB).

       UPDATE-BY-FEE-TYPE.
           ADD TY-FEE-NO-ELIG(SUB) TO BY-FEE-NO-ELIG(1 SUB)
                                      BY-FEE-NO-ELIG(2 SUB).
           ADD TY-FEE-NO-WROTE(SUB) TO BY-FEE-NO-WROTE(1 SUB)
                                       BY-FEE-NO-WROTE(2 SUB).
           ADD TY-FEE-NO-NOTWROTE(SUB) TO BY-FEE-NO-NOTWROTE(1 SUB)
                                          BY-FEE-NO-NOTWROTE(2 SUB).
           ADD TY-FEE-AMT-WROTE(SUB) TO BY-FEE-AMT-WROTE(1 SUB)
                                        BY-FEE-AMT-WROTE(2 SUB).

       UPDATE-BY-ACTIVITY.
           PERFORM UPDATE-BY-TYPE VARYING
              TYP FROM 1 BY 1 UNTIL TYP > 11.

       UPDATE-BY-TYPE.
           ADD TY-NO(ACT TYP) TO BY-NO(1 ACT TYP).
           ADD TY-AMT(ACT TYP) TO BY-AMT(1 ACT TYP).
           ADD TY-O2(ACT TYP) TO BY-O2(1 ACT TYP).
           ADD TY-INT(ACT TYP) TO BY-INT(1 ACT TYP).

           IF TYP < 11
              ADD TY-NO-PL(ACT TYP) TO BY-NO-PL(1 ACT TYP)
              ADD TY-AMT-PL(ACT TYP) TO BY-AMT-PL(1 ACT TYP)
              ADD TY-O2-PL(ACT TYP) TO BY-O2-PL(1 ACT TYP)
              ADD TY-INT-PL(ACT TYP) TO BY-INT-PL(1 ACT TYP).
           IF TYP < 6
              ADD TY-NO-REPO(ACT TYP) TO BY-NO-REPO(1 ACT TYP)
              ADD TY-AMT-REPO(ACT TYP) TO BY-AMT-REPO(1 ACT TYP)
              ADD TY-O2-REPO(ACT TYP) TO BY-O2-REPO(1 ACT TYP)
              ADD TY-INT-REPO(ACT TYP) TO BY-INT-REPO(1 ACT TYP).
           IF TYP < 4
              ADD TY-DLRVPRIN(ACT TYP) TO BY-DLRVPRIN(1 ACT TYP)
              ADD TY-DLRVCHGS(ACT TYP) TO BY-DLRVCHGS(1 ACT TYP).

           IF ACT = 2
              ADD TY-NO(ACT TYP) TO BY-NO(2 ACT TYP)
              ADD TY-AMT(ACT TYP) TO BY-AMT(2 ACT TYP)
              ADD TY-O2(ACT TYP) TO BY-O2(2 ACT TYP)
              ADD TY-INT(ACT TYP) TO BY-INT(2 ACT TYP)
              IF TYP < 11
                 ADD TY-NO-PL(ACT TYP) TO BY-NO-PL(2 ACT TYP)
                 ADD TY-AMT-PL(ACT TYP) TO BY-AMT-PL(2 ACT TYP)
                 ADD TY-O2-PL(ACT TYP) TO BY-O2-PL(2 ACT TYP)
                 ADD TY-INT-PL(ACT TYP) TO BY-INT-PL(2 ACT TYP)
              END-IF
              IF TYP < 6
                 ADD TY-NO-REPO(ACT TYP) TO BY-NO-REPO(2 ACT TYP)
                 ADD TY-AMT-REPO(ACT TYP) TO BY-AMT-REPO(2 ACT TYP)
                 ADD TY-O2-REPO(ACT TYP) TO BY-O2-REPO(2 ACT TYP)
                 ADD TY-INT-REPO(ACT TYP) TO BY-INT-REPO(2 ACT TYP)
              END-IF
              IF TYP < 4
                 ADD TY-DLRVPRIN(ACT TYP) TO BY-DLRVPRIN(2 ACT TYP)
                 ADD TY-DLRVCHGS(ACT TYP) TO BY-DLRVCHGS(2 ACT TYP).

       UPDATE-BY-DEL.
           ADD TY-UNITS(SUB2 SUB3) TO BY-UNITS(1 SUB2 SUB3).
           ADD TY-OUTBAL(SUB2 SUB3) TO BY-OUTBAL(1 SUB2 SUB3).
           ADD TY-UNITS-BNKRPT(SUB2 SUB3)
                               TO BY-UNITS-BNKRPT(1 SUB2 SUB3).
           ADD TY-OUTBAL-BNKRPT(SUB2 SUB3)
                               TO BY-OUTBAL-BNKRPT(1 SUB2 SUB3).
       EXIT-ADD-BY-FILE.
           EXIT.

      ***********************************************
       SUBTRACT-BY1-FILE SECTION.
      ***********************************************
           SUBTRACT TY-DLPARVHELD(1) FROM BY-DLPARVHELD(1 1).
           SUBTRACT TY-DLPARVHELD(2) FROM BY-DLPARVHELD(1 2)
                                          BY-DLPARVHELD(2 2).
           SUBTRACT TY-DLDISC    FROM BY-DLDISC(1) BY-DLDISC(2).
           SUBTRACT TY-AMT-SOLD  FROM BY-AMT-SOLD(1) BY-AMT-SOLD(2).
           SUBTRACT TY-AMT-TRANS FROM BY-AMT-TRANS(1) BY-AMT-TRANS(2).
           SUBTRACT TY-AMT-PURCH FROM BY-AMT-PURCH(1) BY-AMT-PURCH(2).
           SUBTRACT TY-NO-SOLD   FROM BY-NO-SOLD(1) BY-NO-SOLD(2).
           SUBTRACT TY-NO-TRANS  FROM BY-NO-TRANS(1) BY-NO-TRANS(2).
           SUBTRACT TY-NO-PURCH  FROM BY-NO-PURCH(1) BY-NO-PURCH(2).
           SUBTRACT TY-OTHCOL    FROM BY-OTHCOL(1) BY-OTHCOL(2).
           SUBTRACT TY-OT2COL    FROM BY-OT2COL(1) BY-OT2COL(2).
           SUBTRACT TY-INTCOL    FROM BY-INTCOL(1) BY-INTCOL(2).
           SUBTRACT TY-LATECOL   FROM BY-LATECOL(1) BY-LATECOL(2).
           SUBTRACT TY-PRINCOL   FROM BY-PRINCOL(1) BY-PRINCOL(2).
           SUBTRACT TY-DEFCOL    FROM BY-DEFCOL(1) BY-DEFCOL(2).
           SUBTRACT TY-RNPRIN    FROM BY-RNPRIN(1) BY-RNPRIN(2).
           SUBTRACT TY-INTREB    FROM BY-INTREB(1) BY-INTREB(2).
           SUBTRACT TY-CLREB1    FROM BY-CLREB1(1) BY-CLREB1(2).
           SUBTRACT TY-CLREB2    FROM BY-CLREB2(1) BY-CLREB2(2).
           SUBTRACT TY-CLREB3    FROM BY-CLREB3(1) BY-CLREB3(2).
           SUBTRACT TY-CLREB4    FROM BY-CLREB4(1) BY-CLREB4(2).
           SUBTRACT TY-AHREB     FROM BY-AHREB(1) BY-AHREB(2).
           SUBTRACT TY-PPREB     FROM BY-PPREB(1) BY-PPREB(2).
           SUBTRACT TY-O1REB     FROM BY-O1REB(1) BY-O1REB(2).
           SUBTRACT TY-O2REB     FROM BY-O2REB(1) BY-O2REB(2).
           SUBTRACT TY-O3REB     FROM BY-O3REB(1) BY-O3REB(2).
           SUBTRACT TY-O4REB     FROM BY-O4REB(1) BY-O4REB(2).
           SUBTRACT TY-O5REB     FROM BY-O5REB(1) BY-O5REB(2).
           SUBTRACT TY-O6REB     FROM BY-O6REB(1) BY-O6REB(2).
           SUBTRACT TY-O7REB     FROM BY-O7REB(1) BY-O7REB(2).
           SUBTRACT TY-SCREB     FROM BY-SCREB(1) BY-SCREB(2).
           SUBTRACT TY-MFREB     FROM BY-MFREB(1) BY-MFREB(2).
           SUBTRACT TY-DFREB     FROM BY-DFREB(1) BY-DFREB(2).
           SUBTRACT TY-PART-CHGOFF FROM BY-PART-CHGOFF(1)
                                        BY-PART-CHGOFF(2).
           SUBTRACT TY-ALLOTMENTS FROM BY-ALLOTMENTS(1)
                                       BY-ALLOTMENTS(2).
           SUBTRACT TY-SUMWGTTM FROM BY-SUMWGTTM.
           SUBTRACT TY-SUMWGTAV FROM BY-SUMWGTAV.
           SUBTRACT TY-SUMWGTTA FROM BY-SUMWGTTA.

           SUBTRACT TY-OTH-NO-COL FROM BY-OTH-NO-COL(1)
                                       BY-OTH-NO-COL(2).
           SUBTRACT TY-OT2-NO-COL FROM BY-OT2-NO-COL(1)
                                       BY-OT2-NO-COL(2).
           SUBTRACT TY-INT-NO-COL FROM BY-INT-NO-COL(1)
                                       BY-INT-NO-COL(2).
           SUBTRACT TY-LATE-NO-COL FROM BY-LATE-NO-COL(1)
                                        BY-LATE-NO-COL(2).

           SUBTRACT TY-OTH-NO-WAIVED FROM BY-OTH-NO-WAIVED(1)
                                          BY-OTH-NO-WAIVED(2).
           SUBTRACT TY-OT2-NO-WAIVED FROM BY-OT2-NO-WAIVED(1)
                                          BY-OT2-NO-WAIVED(2).
           SUBTRACT TY-INT-NO-WAIVED FROM BY-INT-NO-WAIVED(1)
                                          BY-INT-NO-WAIVED(2).
           SUBTRACT TY-LATE-NO-WAIVED FROM BY-LATE-NO-WAIVED(1)
                                           BY-LATE-NO-WAIVED(2).

           SUBTRACT TY-OTH-AMT-WAIVED FROM BY-OTH-AMT-WAIVED(1)
                                           BY-OTH-AMT-WAIVED(2).
           SUBTRACT TY-OT2-AMT-WAIVED FROM BY-OT2-AMT-WAIVED(1)
                                           BY-OT2-AMT-WAIVED(2).
           SUBTRACT TY-INT-AMT-WAIVED FROM BY-INT-AMT-WAIVED(1)
                                           BY-INT-AMT-WAIVED(2).
           SUBTRACT TY-LATE-AMT-WAIVED FROM BY-LATE-AMT-WAIVED(1)
                                            BY-LATE-AMT-WAIVED(2).

           PERFORM SUBTRACT-BY-PL-RECOVERED VARYING
              SUB FROM 1 BY 1 UNTIL SUB > 5.

           PERFORM SUBTRACT-BY-INS-TYPE VARYING
              SUB FROM 1 BY 1 UNTIL SUB > 10.

           PERFORM SUBTRACT-BY-ACTIVITY VARYING
              ACT FROM 1 BY 1 UNTIL ACT > 2.

           PERFORM SUBTRACT-BY-DEL
              VARYING SUB3 FROM 1 BY 1
                             UNTIL SUB3 > 10
                AFTER SUB2 FROM 1 BY 1
                                  UNTIL SUB2 > 2.

           GO TO EXIT-SUBTRACT-BY-FILE.

       SUBTRACT-BY-PL-RECOVERED.
           SUBTRACT TY-PL-PRINCOL(SUB) FROM BY-PL-PRINCOL(1 SUB)
                                            BY-PL-PRINCOL(2 SUB).
           SUBTRACT TY-PL-INTCOL(SUB) FROM BY-PL-INTCOL(1 SUB)
                                           BY-PL-INTCOL(2 SUB).
           SUBTRACT TY-PL-LATECOL(SUB) FROM BY-PL-LATECOL(1 SUB)
                                            BY-PL-LATECOL(2 SUB).
           SUBTRACT TY-PL-O2COL(SUB) FROM BY-PL-O2COL(1 SUB)
                                          BY-PL-O2COL(2 SUB).

       SUBTRACT-BY-INS-TYPE.
           SUBTRACT TY-INS-NO-ELIG(SUB) FROM BY-INS-NO-ELIG(1 SUB)
                                             BY-INS-NO-ELIG(2 SUB).
           SUBTRACT TY-INS-NO-WROTE(SUB) FROM BY-INS-NO-WROTE(1 SUB)
                                              BY-INS-NO-WROTE(2 SUB).
           SUBTRACT TY-INS-NO-NOTWROTE(SUB) FROM
                                              BY-INS-NO-NOTWROTE(1 SUB)
                                              BY-INS-NO-NOTWROTE(2 SUB).
           SUBTRACT TY-INS-AMT-WROTE(SUB) FROM BY-INS-AMT-WROTE(1 SUB)
                                               BY-INS-AMT-WROTE(2 SUB).

       SUBTRACT-BY-ACTIVITY.
           PERFORM SUBTRACT-BY-TYPE VARYING
              TYP FROM 1 BY 1 UNTIL TYP > 11.

       SUBTRACT-BY-TYPE.
           SUBTRACT TY-NO(ACT TYP) FROM BY-NO(1 ACT TYP).
           SUBTRACT TY-AMT(ACT TYP) FROM BY-AMT(1 ACT TYP).
           SUBTRACT TY-O2(ACT TYP) FROM BY-O2(1 ACT TYP).
           SUBTRACT TY-INT(ACT TYP) FROM BY-INT(1 ACT TYP).

           IF TYP < 11
              SUBTRACT TY-NO-PL(ACT TYP) FROM BY-NO-PL(1 ACT TYP)
              SUBTRACT TY-AMT-PL(ACT TYP) FROM BY-AMT-PL(1 ACT TYP)
              SUBTRACT TY-O2-PL(ACT TYP) FROM BY-O2-PL(1 ACT TYP)
              SUBTRACT TY-INT-PL(ACT TYP) FROM BY-INT-PL(1 ACT TYP).
           IF TYP < 6
              SUBTRACT TY-NO-REPO(ACT TYP) FROM BY-NO-REPO(1 ACT TYP)
              SUBTRACT TY-AMT-REPO(ACT TYP) FROM BY-AMT-REPO(1 ACT TYP)
              SUBTRACT TY-O2-REPO(ACT TYP) FROM BY-O2-REPO(1 ACT TYP)
              SUBTRACT TY-INT-REPO(ACT TYP) FROM BY-INT-REPO(1 ACT TYP).
           IF TYP < 4
              SUBTRACT TY-DLRVPRIN(ACT TYP) FROM BY-DLRVPRIN(1 ACT TYP)
              SUBTRACT TY-DLRVCHGS(ACT TYP) FROM BY-DLRVCHGS(1 ACT TYP).

           IF ACT = 2
              SUBTRACT TY-NO(ACT TYP) FROM BY-NO(2 ACT TYP)
              SUBTRACT TY-AMT(ACT TYP) FROM BY-AMT(2 ACT TYP)
              SUBTRACT TY-O2(ACT TYP) FROM BY-O2(2 ACT TYP)
              SUBTRACT TY-INT(ACT TYP) FROM BY-INT(2 ACT TYP)
              IF TYP < 11
                 SUBTRACT TY-NO-PL(ACT TYP) FROM BY-NO-PL(2 ACT TYP)
                 SUBTRACT TY-AMT-PL(ACT TYP) FROM BY-AMT-PL(2 ACT TYP)
                 SUBTRACT TY-O2-PL(ACT TYP) FROM BY-O2-PL(2 ACT TYP)
                 SUBTRACT TY-INT-PL(ACT TYP) FROM BY-INT-PL(2 ACT TYP)
              END-IF
              IF TYP < 6
                 SUBTRACT TY-NO-REPO(ACT TYP) FROM
                          BY-NO-REPO(2 ACT TYP)
                 SUBTRACT TY-AMT-REPO(ACT TYP) FROM
                          BY-AMT-REPO(2 ACT TYP)
                 SUBTRACT TY-O2-REPO(ACT TYP) FROM
                          BY-O2-REPO(2 ACT TYP)
                 SUBTRACT TY-INT-REPO(ACT TYP) FROM
                          BY-INT-REPO(2 ACT TYP)
              END-IF
              IF TYP < 4
                 SUBTRACT TY-DLRVPRIN(ACT TYP) FROM
                          BY-DLRVPRIN(2 ACT TYP)
                 SUBTRACT TY-DLRVCHGS(ACT TYP) FROM
                          BY-DLRVCHGS(2 ACT TYP).

       SUBTRACT-BY-DEL.
           SUBTRACT TY-UNITS(SUB2 SUB3) FROM BY-UNITS(1 SUB2 SUB3).
           SUBTRACT TY-OUTBAL(SUB2 SUB3) FROM BY-OUTBAL(1 SUB2 SUB3).
           SUBTRACT TY-UNITS-BNKRPT(SUB2 SUB3)
                               FROM BY-UNITS-BNKRPT(1 SUB2 SUB3).
           SUBTRACT TY-OUTBAL-BNKRPT(SUB2 SUB3)
                               FROM BY-OUTBAL-BNKRPT(1 SUB2 SUB3).
       EXIT-SUBTRACT-BY-FILE.
           EXIT.

      ***********************************************
       CREATE-BY1-REC SECTION.
      ***********************************************
           PERFORM CLEAR-BY-FILE.
           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM.
           MOVE TY-BRNO     TO BY-BRNO.
           MOVE TY-CLASS    TO BY-CLASS.
           PERFORM WRITE-BY1-FILE.
           IF IO-FG = 9
              MOVE "WRITE ERROR ON BYFILE" TO MESS
              PERFORM SEND-MESS.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPBYCL.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO FXBYTY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXBYTY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FXBYTY-SCREEN UPON FXBYTY-WINDOW-HANDLE.
           MOVE FXBYTY-FORM-FIELDS TO WR-BUF.
           MOVE FXBYTY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE FXBYTY-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXBYTY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXBYTY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************
       MISC-PARA SECTION.
      ***************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPBY1IN.CPY".
       COPY "LIBLP/LPTY1IN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXBYTY-SCREEN.
           DESTROY FXBYTY-WINDOW-HANDLE.
           EXIT PROGRAM.

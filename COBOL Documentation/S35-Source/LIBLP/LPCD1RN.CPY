      * COPYMEMBER: LIBLP/LPCD1RN
       LOAD-CD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CD-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CD-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CD-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CD-ALL-BASE
              MOVE FILPATH-MACHINE         TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH.

      *-----------------------------------------------------------------
       OPEN-CD1-FILE.
           PERFORM LOAD-CD1-FILE.
           PERFORM OPEN-IT.
           MOVE CD-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CD1-FILE.
           PERFORM START-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.

           IF ( CD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CD1-FILE.

           IF ( QCD1-WEND-TYPE = SPACES    ) OR
              ( QCD1-WEND-CODE = SPACES    )
              MOVE CD-TYPE              TO QCD1-WBEG-TYPE
              MOVE ALL "Z"              TO QCD1-WEND-TYPE
              MOVE CD-CODE              TO QCD1-WBEG-CODE
              MOVE ALL "Z"              TO QCD1-WEND-CODE.

           EXEC SQL
            DECLARE CD1_CURSOR CURSOR FOR
            SELECT CDFILE.CD_TYPE,
                   CDFILE.CD_CODE,
                   CAST(CDFILE.CD_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CDFILE.CD_DESC,
                   CDFILE.CD_AT_ACTIONCD,
                   CDFILE.CD_AT_MEMO_REQUIRED,
                   CDFILE.CD_AT_SUPERVISOR_ONLY,
                   CDFILE.CD_AT_RESULT_REQUIRED,
                   CDFILE.CD_BR_FILE_TYPE,
                   CDFILE.CD_BR_FILE_NAME,
                   CDFILE.CD_BR_PAYOFF_FG,
                   CDFILE.CD_BR_POST_SS_TRCD,
                   CDFILE.CD_BR_GL_BRANCH,
                   CDFILE.CD_BR_GL_ACCT,
                   CDFILE.CD_BR_ALLOT_OPTION,
                   CDFILE.CD_BR_LBOX_OPTION,
                   CDFILE.CD_BR_LC_FG,
                   CDFILE.CD_BR_OT_GL_BRANCH,
                   CDFILE.CD_BR_OT_GL_ACCT,
                   CDFILE.CD_BR_MULTIPLE_REFCDS,
                   CDFILE.CD_BR_COLLECT_LC,
                   CDFILE.CD_CO_GROUP,
                   CDFILE.CD_CO_SECURED,
                   CDFILE.CD_CO_TRW,
                   CDFILE.CD_CT_INSTAXFRMLA_1,
                   CDFILE.CD_CT_INSTAXAMT_1,
                   CDFILE.CD_CT_INSTAXFRMLA_2,
                   CDFILE.CD_CT_INSTAXAMT_2,
                   CDFILE.CD_CT_INSTAXFRMLA_3,
                   CDFILE.CD_CT_INSTAXAMT_3,
                   CDFILE.CD_CT_INSTAXFRMLA_4,
                   CDFILE.CD_CT_INSTAXAMT_4,
                   CDFILE.CD_CT_INSTAXFRMLA_5,
                   CDFILE.CD_CT_INSTAXAMT_5,
                   CDFILE.CD_CT_INSTAXFRMLA_6,
                   CDFILE.CD_CT_INSTAXAMT_6,
                   CDFILE.CD_CT_INSTAXFRMLA_7,
                   CDFILE.CD_CT_INSTAXAMT_7,
                   CDFILE.CD_CT_INSTAXFRMLA_8,
                   CDFILE.CD_CT_INSTAXAMT_8,
                   CDFILE.CD_CT_INSTAXFRMLA_9,
                   CDFILE.CD_CT_INSTAXAMT_9,
                   CDFILE.CD_CT_INSTAXFRMLA_10,
                   CDFILE.CD_CT_INSTAXAMT_10,
                   CDFILE.CD_CT_INSTAXFRMLA_11,
                   CDFILE.CD_CT_INSTAXAMT_11,
                   CDFILE.CD_CT_INSTAXFRMLA_12,
                   CDFILE.CD_CT_INSTAXAMT_12,
                   CDFILE.CD_CT_BOTH_TAX_FG,
                   CDFILE.CD_CY_INSTAXFRMLA_1,
                   CDFILE.CD_CY_INSTAXAMT_1,
                   CDFILE.CD_CY_INSTAXFRMLA_2,
                   CDFILE.CD_CY_INSTAXAMT_2,
                   CDFILE.CD_CY_INSTAXFRMLA_3,
                   CDFILE.CD_CY_INSTAXAMT_3,
                   CDFILE.CD_CY_INSTAXFRMLA_4,
                   CDFILE.CD_CY_INSTAXAMT_4,
                   CDFILE.CD_CY_INSTAXFRMLA_5,
                   CDFILE.CD_CY_INSTAXAMT_5,
                   CDFILE.CD_CY_INSTAXFRMLA_6,
                   CDFILE.CD_CY_INSTAXAMT_6,
                   CDFILE.CD_CY_INSTAXFRMLA_7,
                   CDFILE.CD_CY_INSTAXAMT_7,
                   CDFILE.CD_CY_INSTAXFRMLA_8,
                   CDFILE.CD_CY_INSTAXAMT_8,
                   CDFILE.CD_CY_INSTAXFRMLA_9,
                   CDFILE.CD_CY_INSTAXAMT_9,
                   CDFILE.CD_CY_INSTAXFRMLA_10,
                   CDFILE.CD_CY_INSTAXAMT_10,
                   CDFILE.CD_CY_INSTAXFRMLA_11,
                   CDFILE.CD_CY_INSTAXAMT_11,
                   CDFILE.CD_CY_INSTAXFRMLA_12,
                   CDFILE.CD_CY_INSTAXAMT_12,
                   CDFILE.CD_CY_BOTH_TAX_FG,
                   CDFILE.CD_EA_CQ_FG,
                   CDFILE.CD_EA_GQ_FG,
                   CDFILE.CD_ELGRACE,
                   CDFILE.CD_ELGRACECD,
                   CDFILE.CD_ELTYPE,
                   CDFILE.CD_ELRATE,
                   CDFILE.CD_ELFRMLA,
                   CDFILE.CD_ELMINCHG,
                   CDFILE.CD_ELMAXAMT,
                   CDFILE.CD_ELDELFAC,
                   CDFILE.CD_ELYRTYPE,
                   CDFILE.CD_ELUNITPER_CD,
                   CDFILE.CD_PO_TITLE_HOLD_DAYS,
                   CDFILE.CD_RE_ACTIONCD,
                   CDFILE.CD_RE_MEMO_REQUIRED,
                   CDFILE.CD_RE_SUPERVISOR_ONLY,
                   CDFILE.CD_RE_ACTIVITY_REQUIRED,
                   CDFILE.CD_RE_RESCHEDULE_REQ,
                   CDFILE.CD_RE_RESCHEDULE_TIME,
                   CDFILE.CD_RE_WORKED_ACCOUNT,
                   CDFILE.CD_FC_FEEAMT,
                   CDFILE.CD_TC_REFNO_1,
                   CDFILE.CD_TC_REFNO_2,
                   CDFILE.CD_TC_REFNO_3,
                   CDFILE.CD_TC_REFNO_4,
                   CDFILE.CD_TC_REFNO_5,
                   CDFILE.CD_TC_REFNO_6,
                   CDFILE.CD_TC_REFNO_7,
                   CDFILE.CD_TC_REFNO_8,
                   CDFILE.CD_TC_REFNO_9,
                   CDFILE.CD_TC_REFNO_10,
                   CDFILE.CD_PS_TDR_FLAG
            FROM DBO.CDFILE
            WHERE CDFILE.CD_TYPE >= :QCD1-WBEG-TYPE
              AND CDFILE.CD_TYPE <= :QCD1-WEND-TYPE
              AND CDFILE.CD_CODE >= :QCD1-WBEG-CODE
              AND CDFILE.CD_CODE <= :QCD1-WEND-CODE
              ORDER BY CDFILE.CD_TYPE,
                       CDFILE.CD_CODE
           END-EXEC.

           EXEC SQL
               OPEN CD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCD1-WBEG-TYPE
                          QCD1-WEND-TYPE
                          QCD1-WBEG-CODE
                          QCD1-WEND-CODE.

           MOVE STAT---GOOD TO CD1-CURSOR-STAT.

      *------------------------------------------------
       START-CD1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.

           IF ( CD1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-CD1-FILE.

           IF ( QCD1-WEND-TYPE = SPACES    ) OR
              ( QCD1-WEND-CODE = SPACES    )
              MOVE SPACES               TO QCD1-WBEG-TYPE
              MOVE CD-TYPE              TO QCD1-WEND-TYPE
              MOVE SPACES               TO QCD1-WBEG-CODE
              MOVE CD-CODE              TO QCD1-WEND-CODE.

           EXEC SQL
            DECLARE CD1_CURSOR_NG CURSOR FOR
            SELECT CDFILE.CD_TYPE,
                   CDFILE.CD_CODE,
                   CAST(CDFILE.CD_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CDFILE.CD_DESC,
                   CDFILE.CD_AT_ACTIONCD,
                   CDFILE.CD_AT_MEMO_REQUIRED,
                   CDFILE.CD_AT_SUPERVISOR_ONLY,
                   CDFILE.CD_AT_RESULT_REQUIRED,
                   CDFILE.CD_BR_FILE_TYPE,
                   CDFILE.CD_BR_FILE_NAME,
                   CDFILE.CD_BR_PAYOFF_FG,
                   CDFILE.CD_BR_POST_SS_TRCD,
                   CDFILE.CD_BR_GL_BRANCH,
                   CDFILE.CD_BR_GL_ACCT,
                   CDFILE.CD_BR_ALLOT_OPTION,
                   CDFILE.CD_BR_LBOX_OPTION,
                   CDFILE.CD_BR_LC_FG,
                   CDFILE.CD_BR_OT_GL_BRANCH,
                   CDFILE.CD_BR_OT_GL_ACCT,
                   CDFILE.CD_BR_MULTIPLE_REFCDS,
                   CDFILE.CD_BR_COLLECT_LC,
                   CDFILE.CD_CO_GROUP,
                   CDFILE.CD_CO_SECURED,
                   CDFILE.CD_CO_TRW,
                   CDFILE.CD_CT_INSTAXFRMLA_1,
                   CDFILE.CD_CT_INSTAXAMT_1,
                   CDFILE.CD_CT_INSTAXFRMLA_2,
                   CDFILE.CD_CT_INSTAXAMT_2,
                   CDFILE.CD_CT_INSTAXFRMLA_3,
                   CDFILE.CD_CT_INSTAXAMT_3,
                   CDFILE.CD_CT_INSTAXFRMLA_4,
                   CDFILE.CD_CT_INSTAXAMT_4,
                   CDFILE.CD_CT_INSTAXFRMLA_5,
                   CDFILE.CD_CT_INSTAXAMT_5,
                   CDFILE.CD_CT_INSTAXFRMLA_6,
                   CDFILE.CD_CT_INSTAXAMT_6,
                   CDFILE.CD_CT_INSTAXFRMLA_7,
                   CDFILE.CD_CT_INSTAXAMT_7,
                   CDFILE.CD_CT_INSTAXFRMLA_8,
                   CDFILE.CD_CT_INSTAXAMT_8,
                   CDFILE.CD_CT_INSTAXFRMLA_9,
                   CDFILE.CD_CT_INSTAXAMT_9,
                   CDFILE.CD_CT_INSTAXFRMLA_10,
                   CDFILE.CD_CT_INSTAXAMT_10,
                   CDFILE.CD_CT_INSTAXFRMLA_11,
                   CDFILE.CD_CT_INSTAXAMT_11,
                   CDFILE.CD_CT_INSTAXFRMLA_12,
                   CDFILE.CD_CT_INSTAXAMT_12,
                   CDFILE.CD_CT_BOTH_TAX_FG,
                   CDFILE.CD_CY_INSTAXFRMLA_1,
                   CDFILE.CD_CY_INSTAXAMT_1,
                   CDFILE.CD_CY_INSTAXFRMLA_2,
                   CDFILE.CD_CY_INSTAXAMT_2,
                   CDFILE.CD_CY_INSTAXFRMLA_3,
                   CDFILE.CD_CY_INSTAXAMT_3,
                   CDFILE.CD_CY_INSTAXFRMLA_4,
                   CDFILE.CD_CY_INSTAXAMT_4,
                   CDFILE.CD_CY_INSTAXFRMLA_5,
                   CDFILE.CD_CY_INSTAXAMT_5,
                   CDFILE.CD_CY_INSTAXFRMLA_6,
                   CDFILE.CD_CY_INSTAXAMT_6,
                   CDFILE.CD_CY_INSTAXFRMLA_7,
                   CDFILE.CD_CY_INSTAXAMT_7,
                   CDFILE.CD_CY_INSTAXFRMLA_8,
                   CDFILE.CD_CY_INSTAXAMT_8,
                   CDFILE.CD_CY_INSTAXFRMLA_9,
                   CDFILE.CD_CY_INSTAXAMT_9,
                   CDFILE.CD_CY_INSTAXFRMLA_10,
                   CDFILE.CD_CY_INSTAXAMT_10,
                   CDFILE.CD_CY_INSTAXFRMLA_11,
                   CDFILE.CD_CY_INSTAXAMT_11,
                   CDFILE.CD_CY_INSTAXFRMLA_12,
                   CDFILE.CD_CY_INSTAXAMT_12,
                   CDFILE.CD_CY_BOTH_TAX_FG,
                   CDFILE.CD_EA_CQ_FG,
                   CDFILE.CD_EA_GQ_FG,
                   CDFILE.CD_ELGRACE,
                   CDFILE.CD_ELGRACECD,
                   CDFILE.CD_ELTYPE,
                   CDFILE.CD_ELRATE,
                   CDFILE.CD_ELFRMLA,
                   CDFILE.CD_ELMINCHG,
                   CDFILE.CD_ELMAXAMT,
                   CDFILE.CD_ELDELFAC,
                   CDFILE.CD_ELYRTYPE,
                   CDFILE.CD_ELUNITPER_CD,
                   CDFILE.CD_PO_TITLE_HOLD_DAYS,
                   CDFILE.CD_RE_ACTIONCD,
                   CDFILE.CD_RE_MEMO_REQUIRED,
                   CDFILE.CD_RE_SUPERVISOR_ONLY,
                   CDFILE.CD_RE_ACTIVITY_REQUIRED,
                   CDFILE.CD_RE_RESCHEDULE_REQ,
                   CDFILE.CD_RE_RESCHEDULE_TIME,
                   CDFILE.CD_RE_WORKED_ACCOUNT,
                   CDFILE.CD_FC_FEEAMT,
                   CDFILE.CD_TC_REFNO_1,
                   CDFILE.CD_TC_REFNO_2,
                   CDFILE.CD_TC_REFNO_3,
                   CDFILE.CD_TC_REFNO_4,
                   CDFILE.CD_TC_REFNO_5,
                   CDFILE.CD_TC_REFNO_6,
                   CDFILE.CD_TC_REFNO_7,
                   CDFILE.CD_TC_REFNO_8,
                   CDFILE.CD_TC_REFNO_9,
                   CDFILE.CD_TC_REFNO_10,
                   CDFILE.CD_PS_TDR_FLAG
            FROM DBO.CDFILE
            WHERE CDFILE.CD_TYPE >= :QCD1-WBEG-TYPE
              AND CDFILE.CD_TYPE <= :QCD1-WEND-TYPE
              AND CDFILE.CD_CODE >= :QCD1-WBEG-CODE
              AND CDFILE.CD_CODE <= :QCD1-WEND-CODE
            ORDER BY CDFILE.CD_TYPE DESC,
                     CDFILE.CD_CODE DESC
           END-EXEC.

           EXEC SQL
               OPEN CD1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCD1-WBEG-TYPE
                          QCD1-WEND-TYPE
                          QCD1-WBEG-CODE
                          QCD1-WEND-CODE.

           MOVE STAT---GOOD TO CD1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-CD1-FILE.
           PERFORM READ-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           INITIALIZE QCD-REC.

           MOVE CD-TYPE    TO QCD-TYPE.
           MOVE CD-CODE    TO QCD-CODE.

           EXEC SQL
            SELECT CDFILE.CD_TYPE,
                   CDFILE.CD_CODE,
                   CAST(CDFILE.CD_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CDFILE.CD_DESC,
                   CDFILE.CD_AT_ACTIONCD,
                   CDFILE.CD_AT_MEMO_REQUIRED,
                   CDFILE.CD_AT_SUPERVISOR_ONLY,
                   CDFILE.CD_AT_RESULT_REQUIRED,
                   CDFILE.CD_BR_FILE_TYPE,
                   CDFILE.CD_BR_FILE_NAME,
                   CDFILE.CD_BR_PAYOFF_FG,
                   CDFILE.CD_BR_POST_SS_TRCD,
                   CDFILE.CD_BR_GL_BRANCH,
                   CDFILE.CD_BR_GL_ACCT,
                   CDFILE.CD_BR_ALLOT_OPTION,
                   CDFILE.CD_BR_LBOX_OPTION,
                   CDFILE.CD_BR_LC_FG,
                   CDFILE.CD_BR_OT_GL_BRANCH,
                   CDFILE.CD_BR_OT_GL_ACCT,
                   CDFILE.CD_BR_MULTIPLE_REFCDS,
                   CDFILE.CD_BR_COLLECT_LC,
                   CDFILE.CD_CO_GROUP,
                   CDFILE.CD_CO_SECURED,
                   CDFILE.CD_CO_TRW,
                   CDFILE.CD_CT_INSTAXFRMLA_1,
                   CDFILE.CD_CT_INSTAXAMT_1,
                   CDFILE.CD_CT_INSTAXFRMLA_2,
                   CDFILE.CD_CT_INSTAXAMT_2,
                   CDFILE.CD_CT_INSTAXFRMLA_3,
                   CDFILE.CD_CT_INSTAXAMT_3,
                   CDFILE.CD_CT_INSTAXFRMLA_4,
                   CDFILE.CD_CT_INSTAXAMT_4,
                   CDFILE.CD_CT_INSTAXFRMLA_5,
                   CDFILE.CD_CT_INSTAXAMT_5,
                   CDFILE.CD_CT_INSTAXFRMLA_6,
                   CDFILE.CD_CT_INSTAXAMT_6,
                   CDFILE.CD_CT_INSTAXFRMLA_7,
                   CDFILE.CD_CT_INSTAXAMT_7,
                   CDFILE.CD_CT_INSTAXFRMLA_8,
                   CDFILE.CD_CT_INSTAXAMT_8,
                   CDFILE.CD_CT_INSTAXFRMLA_9,
                   CDFILE.CD_CT_INSTAXAMT_9,
                   CDFILE.CD_CT_INSTAXFRMLA_10,
                   CDFILE.CD_CT_INSTAXAMT_10,
                   CDFILE.CD_CT_INSTAXFRMLA_11,
                   CDFILE.CD_CT_INSTAXAMT_11,
                   CDFILE.CD_CT_INSTAXFRMLA_12,
                   CDFILE.CD_CT_INSTAXAMT_12,
                   CDFILE.CD_CT_BOTH_TAX_FG,
                   CDFILE.CD_CY_INSTAXFRMLA_1,
                   CDFILE.CD_CY_INSTAXAMT_1,
                   CDFILE.CD_CY_INSTAXFRMLA_2,
                   CDFILE.CD_CY_INSTAXAMT_2,
                   CDFILE.CD_CY_INSTAXFRMLA_3,
                   CDFILE.CD_CY_INSTAXAMT_3,
                   CDFILE.CD_CY_INSTAXFRMLA_4,
                   CDFILE.CD_CY_INSTAXAMT_4,
                   CDFILE.CD_CY_INSTAXFRMLA_5,
                   CDFILE.CD_CY_INSTAXAMT_5,
                   CDFILE.CD_CY_INSTAXFRMLA_6,
                   CDFILE.CD_CY_INSTAXAMT_6,
                   CDFILE.CD_CY_INSTAXFRMLA_7,
                   CDFILE.CD_CY_INSTAXAMT_7,
                   CDFILE.CD_CY_INSTAXFRMLA_8,
                   CDFILE.CD_CY_INSTAXAMT_8,
                   CDFILE.CD_CY_INSTAXFRMLA_9,
                   CDFILE.CD_CY_INSTAXAMT_9,
                   CDFILE.CD_CY_INSTAXFRMLA_10,
                   CDFILE.CD_CY_INSTAXAMT_10,
                   CDFILE.CD_CY_INSTAXFRMLA_11,
                   CDFILE.CD_CY_INSTAXAMT_11,
                   CDFILE.CD_CY_INSTAXFRMLA_12,
                   CDFILE.CD_CY_INSTAXAMT_12,
                   CDFILE.CD_CY_BOTH_TAX_FG,
                   CDFILE.CD_EA_CQ_FG,
                   CDFILE.CD_EA_GQ_FG,
                   CDFILE.CD_ELGRACE,
                   CDFILE.CD_ELGRACECD,
                   CDFILE.CD_ELTYPE,
                   CDFILE.CD_ELRATE,
                   CDFILE.CD_ELFRMLA,
                   CDFILE.CD_ELMINCHG,
                   CDFILE.CD_ELMAXAMT,
                   CDFILE.CD_ELDELFAC,
                   CDFILE.CD_ELYRTYPE,
                   CDFILE.CD_ELUNITPER_CD,
                   CDFILE.CD_PO_TITLE_HOLD_DAYS,
                   CDFILE.CD_RE_ACTIONCD,
                   CDFILE.CD_RE_MEMO_REQUIRED,
                   CDFILE.CD_RE_SUPERVISOR_ONLY,
                   CDFILE.CD_RE_ACTIVITY_REQUIRED,
                   CDFILE.CD_RE_RESCHEDULE_REQ,
                   CDFILE.CD_RE_RESCHEDULE_TIME,
                   CDFILE.CD_RE_WORKED_ACCOUNT,
                   CDFILE.CD_FC_FEEAMT,
                   CDFILE.CD_TC_REFNO_1,
                   CDFILE.CD_TC_REFNO_2,
                   CDFILE.CD_TC_REFNO_3,
                   CDFILE.CD_TC_REFNO_4,
                   CDFILE.CD_TC_REFNO_5,
                   CDFILE.CD_TC_REFNO_6,
                   CDFILE.CD_TC_REFNO_7,
                   CDFILE.CD_TC_REFNO_8,
                   CDFILE.CD_TC_REFNO_9,
                   CDFILE.CD_TC_REFNO_10,
                   CDFILE.CD_PS_TDR_FLAG
            INTO :QCD-REC
            FROM DBO.CDFILE
            WHERE CDFILE.CD_TYPE = :QCD-TYPE
              AND CDFILE.CD_CODE = :QCD-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
       READ-CD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           INITIALIZE QCD-REC.

           IF ( CD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CD1_CURSOR INTO :QCD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
       READ-CD1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           INITIALIZE QCD-REC.

           IF ( CD1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH CD1_CURSOR_NG INTO :QCD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CD1-FILE.
           PERFORM CLOSE-IT.
           IF ( CD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CD1-CURSOR-STAT.

           IF ( CD1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE CD1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO CD1-CURSOR-NG-STAT.


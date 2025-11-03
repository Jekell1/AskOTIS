      * COPYMEMBER: LIBLP/LPCD1IN
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
       WRITE-CD1-FILE.
           ACCEPT CD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           PERFORM SET-CD-FIELDS.

           EXEC SQL
            INSERT INTO DBO.CDFILE
             (CDFILE.CD_TYPE,
              CDFILE.CD_CODE,
              CDFILE.CD_LTOUCH_DATE,
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
              CDFILE.CD_PS_TDR_FLAG)
            VALUES
             (:QCD-TYPE,
              :QCD-CODE,
              :QCD-LTOUCH-DATE,
              :QCD-DESC,
              :QCD-AT-ACTIONCD,
              :QCD-AT-MEMO-REQUIRED,
              :QCD-AT-SUPERVISOR-ONLY,
              :QCD-AT-RESULT-REQUIRED,
              :QCD-BR-FILE-TYPE,
              :QCD-BR-FILE-NAME,
              :QCD-BR-PAYOFF-FG,
              :QCD-BR-POST-SS-TRCD,
              :QCD-BR-GL-BRANCH,
              :QCD-BR-GL-ACCT,
              :QCD-BR-ALLOT-OPTION,
              :QCD-BR-LBOX-OPTION,
              :QCD-BR-LC-FG,
              :QCD-BR-OT-GL-BRANCH,
              :QCD-BR-OT-GL-ACCT,
              :QCD-BR-MULTIPLE-REFCDS,
              :QCD-BR-COLLECT-LC,
              :QCD-CO-GROUP,
              :QCD-CO-SECURED,
              :QCD-CO-TRW,
              :QCD-CT-INSTAXFRMLA-1,
              :QCD-CT-INSTAXAMT-1,
              :QCD-CT-INSTAXFRMLA-2,
              :QCD-CT-INSTAXAMT-2,
              :QCD-CT-INSTAXFRMLA-3,
              :QCD-CT-INSTAXAMT-3,
              :QCD-CT-INSTAXFRMLA-4,
              :QCD-CT-INSTAXAMT-4,
              :QCD-CT-INSTAXFRMLA-5,
              :QCD-CT-INSTAXAMT-5,
              :QCD-CT-INSTAXFRMLA-6,
              :QCD-CT-INSTAXAMT-6,
              :QCD-CT-INSTAXFRMLA-7,
              :QCD-CT-INSTAXAMT-7,
              :QCD-CT-INSTAXFRMLA-8,
              :QCD-CT-INSTAXAMT-8,
              :QCD-CT-INSTAXFRMLA-9,
              :QCD-CT-INSTAXAMT-9,
              :QCD-CT-INSTAXFRMLA-10,
              :QCD-CT-INSTAXAMT-10,
              :QCD-CT-INSTAXFRMLA-11,
              :QCD-CT-INSTAXAMT-11,
              :QCD-CT-INSTAXFRMLA-12,
              :QCD-CT-INSTAXAMT-12,
              :QCD-CT-BOTH-TAX-FG,
              :QCD-CY-INSTAXFRMLA-1,
              :QCD-CY-INSTAXAMT-1,
              :QCD-CY-INSTAXFRMLA-2,
              :QCD-CY-INSTAXAMT-2,
              :QCD-CY-INSTAXFRMLA-3,
              :QCD-CY-INSTAXAMT-3,
              :QCD-CY-INSTAXFRMLA-4,
              :QCD-CY-INSTAXAMT-4,
              :QCD-CY-INSTAXFRMLA-5,
              :QCD-CY-INSTAXAMT-5,
              :QCD-CY-INSTAXFRMLA-6,
              :QCD-CY-INSTAXAMT-6,
              :QCD-CY-INSTAXFRMLA-7,
              :QCD-CY-INSTAXAMT-7,
              :QCD-CY-INSTAXFRMLA-8,
              :QCD-CY-INSTAXAMT-8,
              :QCD-CY-INSTAXFRMLA-9,
              :QCD-CY-INSTAXAMT-9,
              :QCD-CY-INSTAXFRMLA-10,
              :QCD-CY-INSTAXAMT-10,
              :QCD-CY-INSTAXFRMLA-11,
              :QCD-CY-INSTAXAMT-11,
              :QCD-CY-INSTAXFRMLA-12,
              :QCD-CY-INSTAXAMT-12,
              :QCD-CY-BOTH-TAX-FG,
              :QCD-EA-CQ-FG,
              :QCD-EA-GQ-FG,
              :QCD-ELGRACE,
              :QCD-ELGRACECD,
              :QCD-ELTYPE,
              :QCD-ELRATE,
              :QCD-ELFRMLA,
              :QCD-ELMINCHG,
              :QCD-ELMAXAMT,
              :QCD-ELDELFAC,
              :QCD-ELYRTYPE,
              :QCD-ELUNITPER-CD,
              :QCD-PO-TITLE-HOLD-DAYS,
              :QCD-RE-ACTIONCD,
              :QCD-RE-MEMO-REQUIRED,
              :QCD-RE-SUPERVISOR-ONLY,
              :QCD-RE-ACTIVITY-REQUIRED,
              :QCD-RE-RESCHEDULE-REQ,
              :QCD-RE-RESCHEDULE-TIME,
              :QCD-RE-WORKED-ACCOUNT,
              :QCD-FC-FEEAMT,
              :QCD-TC-REFNO-1,
              :QCD-TC-REFNO-2,
              :QCD-TC-REFNO-3,
              :QCD-TC-REFNO-4,
              :QCD-TC-REFNO-5,
              :QCD-TC-REFNO-6,
              :QCD-TC-REFNO-7,
              :QCD-TC-REFNO-8,
              :QCD-TC-REFNO-9,
              :QCD-TC-REFNO-10,
              :QCD-PS-TDR-FLAG)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-CD1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-CD1-FILE.

           ACCEPT CD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           PERFORM SET-CD-FIELDS.

           EXEC SQL
            UPDATE DBO.CDFILE
             SET
              CDFILE.CD_TYPE = :QCD-TYPE,
              CDFILE.CD_CODE = :QCD-CODE,
              CDFILE.CD_LTOUCH_DATE = :QCD-LTOUCH-DATE,
              CDFILE.CD_DESC = :QCD-DESC,
              CDFILE.CD_AT_ACTIONCD = :QCD-AT-ACTIONCD,
              CDFILE.CD_AT_MEMO_REQUIRED = :QCD-AT-MEMO-REQUIRED,
              CDFILE.CD_AT_SUPERVISOR_ONLY =
                   :QCD-AT-SUPERVISOR-ONLY,
              CDFILE.CD_AT_RESULT_REQUIRED =
                   :QCD-AT-RESULT-REQUIRED,
              CDFILE.CD_BR_FILE_TYPE = :QCD-BR-FILE-TYPE,
              CDFILE.CD_BR_FILE_NAME = :QCD-BR-FILE-NAME,
              CDFILE.CD_BR_PAYOFF_FG = :QCD-BR-PAYOFF-FG,
              CDFILE.CD_BR_POST_SS_TRCD = :QCD-BR-POST-SS-TRCD,
              CDFILE.CD_BR_GL_BRANCH = :QCD-BR-GL-BRANCH,
              CDFILE.CD_BR_GL_ACCT = :QCD-BR-GL-ACCT,
              CDFILE.CD_BR_ALLOT_OPTION = :QCD-BR-ALLOT-OPTION,
              CDFILE.CD_BR_LBOX_OPTION = :QCD-BR-LBOX-OPTION,
              CDFILE.CD_BR_LC_FG = :QCD-BR-LC-FG,
              CDFILE.CD_BR_OT_GL_BRANCH = :QCD-BR-OT-GL-BRANCH,
              CDFILE.CD_BR_OT_GL_ACCT = :QCD-BR-OT-GL-ACCT,
              CDFILE.CD_BR_MULTIPLE_REFCDS =
                   :QCD-BR-MULTIPLE-REFCDS,
              CDFILE.CD_BR_COLLECT_LC = :QCD-BR-COLLECT-LC,
              CDFILE.CD_CO_GROUP = :QCD-CO-GROUP,
              CDFILE.CD_CO_SECURED = :QCD-CO-SECURED,
              CDFILE.CD_CO_TRW = :QCD-CO-TRW,
              CDFILE.CD_CT_INSTAXFRMLA_1 = :QCD-CT-INSTAXFRMLA-1,
              CDFILE.CD_CT_INSTAXAMT_1 = :QCD-CT-INSTAXAMT-1,
              CDFILE.CD_CT_INSTAXFRMLA_2 = :QCD-CT-INSTAXFRMLA-2,
              CDFILE.CD_CT_INSTAXAMT_2 = :QCD-CT-INSTAXAMT-2,
              CDFILE.CD_CT_INSTAXFRMLA_3 = :QCD-CT-INSTAXFRMLA-3,
              CDFILE.CD_CT_INSTAXAMT_3 = :QCD-CT-INSTAXAMT-3,
              CDFILE.CD_CT_INSTAXFRMLA_4 = :QCD-CT-INSTAXFRMLA-4,
              CDFILE.CD_CT_INSTAXAMT_4 = :QCD-CT-INSTAXAMT-4,
              CDFILE.CD_CT_INSTAXFRMLA_5 = :QCD-CT-INSTAXFRMLA-5,
              CDFILE.CD_CT_INSTAXAMT_5 = :QCD-CT-INSTAXAMT-5,
              CDFILE.CD_CT_INSTAXFRMLA_6 = :QCD-CT-INSTAXFRMLA-6,
              CDFILE.CD_CT_INSTAXAMT_6 = :QCD-CT-INSTAXAMT-6,
              CDFILE.CD_CT_INSTAXFRMLA_7 = :QCD-CT-INSTAXFRMLA-7,
              CDFILE.CD_CT_INSTAXAMT_7 = :QCD-CT-INSTAXAMT-7,
              CDFILE.CD_CT_INSTAXFRMLA_8 = :QCD-CT-INSTAXFRMLA-8,
              CDFILE.CD_CT_INSTAXAMT_8 = :QCD-CT-INSTAXAMT-8,
              CDFILE.CD_CT_INSTAXFRMLA_9 = :QCD-CT-INSTAXFRMLA-9,
              CDFILE.CD_CT_INSTAXAMT_9 = :QCD-CT-INSTAXAMT-9,
              CDFILE.CD_CT_INSTAXFRMLA_10 =
                   :QCD-CT-INSTAXFRMLA-10,
              CDFILE.CD_CT_INSTAXAMT_10 = :QCD-CT-INSTAXAMT-10,
              CDFILE.CD_CT_INSTAXFRMLA_11 =
                   :QCD-CT-INSTAXFRMLA-11,
              CDFILE.CD_CT_INSTAXAMT_11 = :QCD-CT-INSTAXAMT-11,
              CDFILE.CD_CT_INSTAXFRMLA_12 =
                   :QCD-CT-INSTAXFRMLA-12,
              CDFILE.CD_CT_INSTAXAMT_12 = :QCD-CT-INSTAXAMT-12,
              CDFILE.CD_CT_BOTH_TAX_FG = :QCD-CT-BOTH-TAX-FG,
              CDFILE.CD_CY_INSTAXFRMLA_1 = :QCD-CY-INSTAXFRMLA-1,
              CDFILE.CD_CY_INSTAXAMT_1 = :QCD-CY-INSTAXAMT-1,
              CDFILE.CD_CY_INSTAXFRMLA_2 = :QCD-CY-INSTAXFRMLA-2,
              CDFILE.CD_CY_INSTAXAMT_2 = :QCD-CY-INSTAXAMT-2,
              CDFILE.CD_CY_INSTAXFRMLA_3 = :QCD-CY-INSTAXFRMLA-3,
              CDFILE.CD_CY_INSTAXAMT_3 = :QCD-CY-INSTAXAMT-3,
              CDFILE.CD_CY_INSTAXFRMLA_4 = :QCD-CY-INSTAXFRMLA-4,
              CDFILE.CD_CY_INSTAXAMT_4 = :QCD-CY-INSTAXAMT-4,
              CDFILE.CD_CY_INSTAXFRMLA_5 = :QCD-CY-INSTAXFRMLA-5,
              CDFILE.CD_CY_INSTAXAMT_5 = :QCD-CY-INSTAXAMT-5,
              CDFILE.CD_CY_INSTAXFRMLA_6 = :QCD-CY-INSTAXFRMLA-6,
              CDFILE.CD_CY_INSTAXAMT_6 = :QCD-CY-INSTAXAMT-6,
              CDFILE.CD_CY_INSTAXFRMLA_7 = :QCD-CY-INSTAXFRMLA-7,
              CDFILE.CD_CY_INSTAXAMT_7 = :QCD-CY-INSTAXAMT-7,
              CDFILE.CD_CY_INSTAXFRMLA_8 = :QCD-CY-INSTAXFRMLA-8,
              CDFILE.CD_CY_INSTAXAMT_8 = :QCD-CY-INSTAXAMT-8,
              CDFILE.CD_CY_INSTAXFRMLA_9 = :QCD-CY-INSTAXFRMLA-9,
              CDFILE.CD_CY_INSTAXAMT_9 = :QCD-CY-INSTAXAMT-9,
              CDFILE.CD_CY_INSTAXFRMLA_10 =
                   :QCD-CY-INSTAXFRMLA-10,
              CDFILE.CD_CY_INSTAXAMT_10 = :QCD-CY-INSTAXAMT-10,
              CDFILE.CD_CY_INSTAXFRMLA_11 =
                   :QCD-CY-INSTAXFRMLA-11,
              CDFILE.CD_CY_INSTAXAMT_11 = :QCD-CY-INSTAXAMT-11,
              CDFILE.CD_CY_INSTAXFRMLA_12 =
                   :QCD-CY-INSTAXFRMLA-12,
              CDFILE.CD_CY_INSTAXAMT_12 = :QCD-CY-INSTAXAMT-12,
              CDFILE.CD_CY_BOTH_TAX_FG = :QCD-CY-BOTH-TAX-FG,
              CDFILE.CD_EA_CQ_FG = :QCD-EA-CQ-FG,
              CDFILE.CD_EA_GQ_FG = :QCD-EA-GQ-FG,
              CDFILE.CD_ELGRACE = :QCD-ELGRACE,
              CDFILE.CD_ELGRACECD = :QCD-ELGRACECD,
              CDFILE.CD_ELTYPE = :QCD-ELTYPE,
              CDFILE.CD_ELRATE = :QCD-ELRATE,
              CDFILE.CD_ELFRMLA = :QCD-ELFRMLA,
              CDFILE.CD_ELMINCHG = :QCD-ELMINCHG,
              CDFILE.CD_ELMAXAMT = :QCD-ELMAXAMT,
              CDFILE.CD_ELDELFAC = :QCD-ELDELFAC,
              CDFILE.CD_ELYRTYPE = :QCD-ELYRTYPE,
              CDFILE.CD_ELUNITPER_CD = :QCD-ELUNITPER-CD,
              CDFILE.CD_PO_TITLE_HOLD_DAYS =
                   :QCD-PO-TITLE-HOLD-DAYS,
              CDFILE.CD_RE_ACTIONCD = :QCD-RE-ACTIONCD,
              CDFILE.CD_RE_MEMO_REQUIRED = :QCD-RE-MEMO-REQUIRED,
              CDFILE.CD_RE_SUPERVISOR_ONLY =
                   :QCD-RE-SUPERVISOR-ONLY,
              CDFILE.CD_RE_ACTIVITY_REQUIRED =
                   :QCD-RE-ACTIVITY-REQUIRED,
              CDFILE.CD_RE_RESCHEDULE_REQ =
                   :QCD-RE-RESCHEDULE-REQ,
              CDFILE.CD_RE_RESCHEDULE_TIME =
                   :QCD-RE-RESCHEDULE-TIME,
              CDFILE.CD_RE_WORKED_ACCOUNT =
                   :QCD-RE-WORKED-ACCOUNT,
              CDFILE.CD_FC_FEEAMT = :QCD-FC-FEEAMT,
              CDFILE.CD_TC_REFNO_1 = :QCD-TC-REFNO-1,
              CDFILE.CD_TC_REFNO_2 = :QCD-TC-REFNO-2,
              CDFILE.CD_TC_REFNO_3 = :QCD-TC-REFNO-3,
              CDFILE.CD_TC_REFNO_4 = :QCD-TC-REFNO-4,
              CDFILE.CD_TC_REFNO_5 = :QCD-TC-REFNO-5,
              CDFILE.CD_TC_REFNO_6 = :QCD-TC-REFNO-6,
              CDFILE.CD_TC_REFNO_7 = :QCD-TC-REFNO-7,
              CDFILE.CD_TC_REFNO_8 = :QCD-TC-REFNO-8,
              CDFILE.CD_TC_REFNO_9 = :QCD-TC-REFNO-9,
              CDFILE.CD_TC_REFNO_10 = :QCD-TC-REFNO-10,
              CDFILE.CD_PS_TDR_FLAG = :QCD-PS-TDR-FLAG
             WHERE CDFILE.CD_TYPE = :QCD-TYPE
               AND CDFILE.CD_CODE = :QCD-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CD1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-CD1-FILE.

           PERFORM DELETE-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           PERFORM SET-CD-FIELDS.

           EXEC SQL
            DELETE FROM DBO.CDFILE
             WHERE CDFILE.CD_TYPE = :QCD-TYPE
               AND CDFILE.CD_CODE = :QCD-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-CD1-FILE.

      *-----------------------------------------------------------------
       CLOSE-CD1-FILE.

           PERFORM CLOSE-IT.

           IF ( CD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CD1-CURSOR-STAT.


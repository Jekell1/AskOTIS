      * COPYMEMBER: LIBGL/GLGI1RN
       LOAD-GI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GI-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GI-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GI-ALL-MACHINE
              MOVE GI-ALL-PATH             TO GI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GI-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GI-ALL-BASE
              MOVE FILPATH-MACHINE         TO GI-ALL-MACHINE
              MOVE GI-ALL-PATH             TO GI-PATH.

      *-----------------------------------------------------------------
       OPEN-GI1-FILE.
           PERFORM LOAD-GI1-FILE.
           PERFORM OPEN-IT.
           MOVE GI-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       COUNT-GI1-FILE.
           PERFORM COUNT-IT.
           MOVE GI-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           MOVE GI-BRANCH      TO QGI-BRANCH.

           EXEC SQL
            SELECT COUNT(*)
              INTO :GI1-ROW-COUNT
              FROM DBO.GIFILE
             WHERE GI_BRANCH = :QGI-BRANCH
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO COUNT-GI1-FILE.

      *-----------------------------------------------------------------
       START-GI1-FILE.
           PERFORM START-IT.
           MOVE GI-PATH-SQL TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.

           IF ( GI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GI1-FILE.

           IF ( QGI1-WEND-CLASS = ZEROES    ) OR
              ( QGI1-WEND-CLASS NOT NUMERIC )
              MOVE GI-BRANCH            TO QGI1-WBEG-BRANCH
              MOVE ALL "9"              TO QGI1-WEND-BRANCH
              MOVE GI-CLASS             TO QGI1-WBEG-CLASS
              MOVE ALL "9"              TO QGI1-WEND-CLASS.

           EXEC SQL
            DECLARE GI1_CURSOR CURSOR FOR
            SELECT GIFILE.GI_BRANCH,
                   GIFILE.GI_CLASS,
                   CAST(GIFILE.GI_LTOUCH_DATE AS VARCHAR(10)),
                   GIFILE.GI_CASH,
                   GIFILE.GI_BANK,
                   GIFILE.GI_NSF,
                   GIFILE.GI_AR,
                   GIFILE.GI_PL_1,
                   GIFILE.GI_PL_2,
                   GIFILE.GI_PL_3,
                   GIFILE.GI_PL_4,
                   GIFILE.GI_PL_5,
                   GIFILE.GI_DLINSREB,
                   GIFILE.GI_ACR_INT_AR_AR,
                   GIFILE.GI_OTHBR_REC,
                   GIFILE.GI_OTHBR_PAY,
                   GIFILE.GI_UNINTCHG,
                   GIFILE.GI_UNADDON,
                   GIFILE.GI_UNSERCHG,
                   GIFILE.GI_UNMAINTFEE,
                   GIFILE.GI_UNDLDISC,
                   GIFILE.GI_UNOTHER,
                   GIFILE.GI_UN7,
                   GIFILE.GI_UN8,
                   GIFILE.GI_UN9,
                   GIFILE.GI_UN10,
                   GIFILE.GI_UNCL1,
                   GIFILE.GI_UNCL2,
                   GIFILE.GI_UNAH,
                   GIFILE.GI_UNPP,
                   GIFILE.GI_UNO1,
                   GIFILE.GI_UNO2,
                   GIFILE.GI_UNO3,
                   GIFILE.GI_UNO4,
                   GIFILE.GI_UNO5,
                   GIFILE.GI_UNO6,
                   GIFILE.GI_UNO7,
                   GIFILE.GI_UNINTREPO,
                   GIFILE.GI_UNADDONINTREPO,
                   GIFILE.GI_PLEXP_1,
                   GIFILE.GI_PLEXP_2,
                   GIFILE.GI_PLEXP_3,
                   GIFILE.GI_PLEXP_4,
                   GIFILE.GI_PLEXP_5,
                   GIFILE.GI_DLPARVEX,
                   GIFILE.GI_PLOFFSET_1,
                   GIFILE.GI_PLOFFSET_2,
                   GIFILE.GI_PLOFFSET_3,
                   GIFILE.GI_PLOFFSET_4,
                   GIFILE.GI_PLOFFSET_5,
                   GIFILE.GI_SERVOFF,
                   GIFILE.GI_PAYCL1,
                   GIFILE.GI_PAYCL2,
                   GIFILE.GI_PAYAH,
                   GIFILE.GI_PAYPP,
                   GIFILE.GI_PAYO1,
                   GIFILE.GI_PAYO2,
                   GIFILE.GI_PAYO3,
                   GIFILE.GI_PAYO4,
                   GIFILE.GI_PAYO5,
                   GIFILE.GI_PAYO6,
                   GIFILE.GI_PAYO7,
                   GIFILE.GI_DLPAYABLE,
                   GIFILE.GI_DLRVPRIN_1,
                   GIFILE.GI_DLRVPRIN_2,
                   GIFILE.GI_DLRVPRIN_3,
                   GIFILE.GI_DLRVCHGS_1,
                   GIFILE.GI_DLRVCHGS_2,
                   GIFILE.GI_DLRVCHGS_3,
                   GIFILE.GI_DLPARV,
                   GIFILE.GI_PAYALLOT,
                   GIFILE.GI_DIRECT_PAY,
                   GIFILE.GI_ERINTCHG,
                   GIFILE.GI_ERADDON,
                   GIFILE.GI_ERSERCHG,
                   GIFILE.GI_ERMAINTFEE,
                   GIFILE.GI_ERDLDISC,
                   GIFILE.GI_EROTHER,
                   GIFILE.GI_EARN7,
                   GIFILE.GI_EARN8,
                   GIFILE.GI_EARN9,
                   GIFILE.GI_EARN10,
                   GIFILE.GI_LATECHG,
                   GIFILE.GI_INTEREST,
                   GIFILE.GI_DEFERMENTS,
                   GIFILE.GI_SAPRINCIPAL,
                   GIFILE.GI_PLINT_1,
                   GIFILE.GI_PLINT_2,
                   GIFILE.GI_PLINT_3,
                   GIFILE.GI_PLINT_4,
                   GIFILE.GI_PLINT_5,
                   GIFILE.GI_PLREC_1,
                   GIFILE.GI_PLREC_2,
                   GIFILE.GI_PLREC_3,
                   GIFILE.GI_PLREC_4,
                   GIFILE.GI_PLREC_5,
                   GIFILE.GI_CL1,
                   GIFILE.GI_CL2,
                   GIFILE.GI_AH,
                   GIFILE.GI_PP,
                   GIFILE.GI_O1,
                   GIFILE.GI_O2,
                   GIFILE.GI_O3,
                   GIFILE.GI_O4,
                   GIFILE.GI_O5,
                   GIFILE.GI_O6,
                   GIFILE.GI_O7,
                   GIFILE.GI_PLNEW_AR_1,
                   GIFILE.GI_PLNEW_AR_2,
                   GIFILE.GI_PLNEW_AR_3,
                   GIFILE.GI_PLNEW_AR_4,
                   GIFILE.GI_PLNEW_AR_5,
                   GIFILE.GI_REPO_AR,
                   GIFILE.GI_REPO_INT,
                   GIFILE.GI_ACR_INT_AR,
                   GIFILE.GI_CONV_OFFSET,
                   GIFILE.GI_PARTIAL_PLEXP,
                   GIFILE.GI_PAYLBOX,
                   GIFILE.GI_PENALTY_INT,
                   GIFILE.GI_PAYTPAY,
                   GIFILE.GI_PAYTCOLL,
                   GIFILE.GI_PAYMGRAM,
                   GIFILE.GI_REPO_ADDON_EXP,
                   GIFILE.GI_ACH_BANK,
                   GIFILE.GI_LN_APPROVAL_LIMIT
            FROM DBO.GIFILE
            WHERE GIFILE.GI_BRANCH >= :QGI1-WBEG-BRANCH
              AND GIFILE.GI_BRANCH <= :QGI1-WEND-BRANCH
              AND GIFILE.GI_CLASS >= :QGI1-WBEG-CLASS
              AND GIFILE.GI_CLASS <= :QGI1-WEND-CLASS
            ORDER BY GIFILE.GI_BRANCH,
                     GIFILE.GI_CLASS
           END-EXEC.

           EXEC SQL
               OPEN GI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGI1-WBEG-BRANCH
                          QGI1-WBEG-CLASS
                          QGI1-WEND-BRANCH
                          QGI1-WEND-CLASS.

           MOVE STAT---GOOD TO GI1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GI1-FILE.
           PERFORM READ-IT.
           MOVE GI-PATH-SQL TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.
           INITIALIZE QGI-REC.

           MOVE GI-BRANCH    TO QGI-BRANCH. 
           MOVE GI-CLASS     TO QGI-CLASS.

           EXEC SQL
            SELECT GIFILE.GI_BRANCH,
                   GIFILE.GI_CLASS,
                   CAST(GIFILE.GI_LTOUCH_DATE AS VARCHAR(10)),
                   GIFILE.GI_CASH,
                   GIFILE.GI_BANK,
                   GIFILE.GI_NSF,
                   GIFILE.GI_AR,
                   GIFILE.GI_PL_1,
                   GIFILE.GI_PL_2,
                   GIFILE.GI_PL_3,
                   GIFILE.GI_PL_4,
                   GIFILE.GI_PL_5,
                   GIFILE.GI_DLINSREB,
                   GIFILE.GI_ACR_INT_AR_AR,
                   GIFILE.GI_OTHBR_REC,
                   GIFILE.GI_OTHBR_PAY,
                   GIFILE.GI_UNINTCHG,
                   GIFILE.GI_UNADDON,
                   GIFILE.GI_UNSERCHG,
                   GIFILE.GI_UNMAINTFEE,
                   GIFILE.GI_UNDLDISC,
                   GIFILE.GI_UNOTHER,
                   GIFILE.GI_UN7,
                   GIFILE.GI_UN8,
                   GIFILE.GI_UN9,
                   GIFILE.GI_UN10,
                   GIFILE.GI_UNCL1,
                   GIFILE.GI_UNCL2,
                   GIFILE.GI_UNAH,
                   GIFILE.GI_UNPP,
                   GIFILE.GI_UNO1,
                   GIFILE.GI_UNO2,
                   GIFILE.GI_UNO3,
                   GIFILE.GI_UNO4,
                   GIFILE.GI_UNO5,
                   GIFILE.GI_UNO6,
                   GIFILE.GI_UNO7,
                   GIFILE.GI_UNINTREPO,
                   GIFILE.GI_UNADDONINTREPO,
                   GIFILE.GI_PLEXP_1,
                   GIFILE.GI_PLEXP_2,
                   GIFILE.GI_PLEXP_3,
                   GIFILE.GI_PLEXP_4,
                   GIFILE.GI_PLEXP_5,
                   GIFILE.GI_DLPARVEX,
                   GIFILE.GI_PLOFFSET_1,
                   GIFILE.GI_PLOFFSET_2,
                   GIFILE.GI_PLOFFSET_3,
                   GIFILE.GI_PLOFFSET_4,
                   GIFILE.GI_PLOFFSET_5,
                   GIFILE.GI_SERVOFF,
                   GIFILE.GI_PAYCL1,
                   GIFILE.GI_PAYCL2,
                   GIFILE.GI_PAYAH,
                   GIFILE.GI_PAYPP,
                   GIFILE.GI_PAYO1,
                   GIFILE.GI_PAYO2,
                   GIFILE.GI_PAYO3,
                   GIFILE.GI_PAYO4,
                   GIFILE.GI_PAYO5,
                   GIFILE.GI_PAYO6,
                   GIFILE.GI_PAYO7,
                   GIFILE.GI_DLPAYABLE,
                   GIFILE.GI_DLRVPRIN_1,
                   GIFILE.GI_DLRVPRIN_2,
                   GIFILE.GI_DLRVPRIN_3,
                   GIFILE.GI_DLRVCHGS_1,
                   GIFILE.GI_DLRVCHGS_2,
                   GIFILE.GI_DLRVCHGS_3,
                   GIFILE.GI_DLPARV,
                   GIFILE.GI_PAYALLOT,
                   GIFILE.GI_DIRECT_PAY,
                   GIFILE.GI_ERINTCHG,
                   GIFILE.GI_ERADDON,
                   GIFILE.GI_ERSERCHG,
                   GIFILE.GI_ERMAINTFEE,
                   GIFILE.GI_ERDLDISC,
                   GIFILE.GI_EROTHER,
                   GIFILE.GI_EARN7,
                   GIFILE.GI_EARN8,
                   GIFILE.GI_EARN9,
                   GIFILE.GI_EARN10,
                   GIFILE.GI_LATECHG,
                   GIFILE.GI_INTEREST,
                   GIFILE.GI_DEFERMENTS,
                   GIFILE.GI_SAPRINCIPAL,
                   GIFILE.GI_PLINT_1,
                   GIFILE.GI_PLINT_2,
                   GIFILE.GI_PLINT_3,
                   GIFILE.GI_PLINT_4,
                   GIFILE.GI_PLINT_5,
                   GIFILE.GI_PLREC_1,
                   GIFILE.GI_PLREC_2,
                   GIFILE.GI_PLREC_3,
                   GIFILE.GI_PLREC_4,
                   GIFILE.GI_PLREC_5,
                   GIFILE.GI_CL1,
                   GIFILE.GI_CL2,
                   GIFILE.GI_AH,
                   GIFILE.GI_PP,
                   GIFILE.GI_O1,
                   GIFILE.GI_O2,
                   GIFILE.GI_O3,
                   GIFILE.GI_O4,
                   GIFILE.GI_O5,
                   GIFILE.GI_O6,
                   GIFILE.GI_O7,
                   GIFILE.GI_PLNEW_AR_1,
                   GIFILE.GI_PLNEW_AR_2,
                   GIFILE.GI_PLNEW_AR_3,
                   GIFILE.GI_PLNEW_AR_4,
                   GIFILE.GI_PLNEW_AR_5,
                   GIFILE.GI_REPO_AR,
                   GIFILE.GI_REPO_INT,
                   GIFILE.GI_ACR_INT_AR,
                   GIFILE.GI_CONV_OFFSET,
                   GIFILE.GI_PARTIAL_PLEXP,
                   GIFILE.GI_PAYLBOX,
                   GIFILE.GI_PENALTY_INT,
                   GIFILE.GI_PAYTPAY,
                   GIFILE.GI_PAYTCOLL,
                   GIFILE.GI_PAYMGRAM,
                   GIFILE.GI_REPO_ADDON_EXP,
                   GIFILE.GI_ACH_BANK,
                   GIFILE.GI_LN_APPROVAL_LIMIT
            INTO :QGI-REC
            FROM DBO.GIFILE
            WHERE GIFILE.GI_BRANCH = :QGI-BRANCH
              AND GIFILE.GI_CLASS = :QGI-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GI1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GI-FIELDS.

      *-----------------------------------------------------------------
       READ-GI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GI-PATH-SQL TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.
           INITIALIZE QGI-REC.

           IF ( GI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GI1_CURSOR INTO :QGI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GI-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GI1-FILE.
           PERFORM CLOSE-IT.
           IF ( GI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GI1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GI1-CURSOR-STAT.


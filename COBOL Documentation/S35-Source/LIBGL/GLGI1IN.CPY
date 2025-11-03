      * COPYMEMBER: LIBGL/GLGI1IN
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
       WRITE-GI1-FILE.

           ACCEPT GI-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GI-PATH-SQL TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.
           PERFORM SET-GI-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GIFILE
             (GIFILE.GI_BRANCH,
              GIFILE.GI_CLASS,
              GIFILE.GI_LTOUCH_DATE,
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
              GIFILE.GI_LN_APPROVAL_LIMIT)
            VALUES
             (:QGI-BRANCH,
              :QGI-CLASS,
              :QGI-LTOUCH-DATE,
              :QGI-CASH,
              :QGI-BANK,
              :QGI-NSF,
              :QGI-AR,
              :QGI-PL-1,
              :QGI-PL-2,
              :QGI-PL-3,
              :QGI-PL-4,
              :QGI-PL-5,
              :QGI-DLINSREB,
              :QGI-ACR-INT-AR-AR,
              :QGI-OTHBR-REC,
              :QGI-OTHBR-PAY,
              :QGI-UNINTCHG,
              :QGI-UNADDON,
              :QGI-UNSERCHG,
              :QGI-UNMAINTFEE,
              :QGI-UNDLDISC,
              :QGI-UNOTHER,
              :QGI-UN7,
              :QGI-UN8,
              :QGI-UN9,
              :QGI-UN10,
              :QGI-UNCL1,
              :QGI-UNCL2,
              :QGI-UNAH,
              :QGI-UNPP,
              :QGI-UNO1,
              :QGI-UNO2,
              :QGI-UNO3,
              :QGI-UNO4,
              :QGI-UNO5,
              :QGI-UNO6,
              :QGI-UNO7,
              :QGI-UNINTREPO,
              :QGI-UNADDONINTREPO,
              :QGI-PLEXP-1,
              :QGI-PLEXP-2,
              :QGI-PLEXP-3,
              :QGI-PLEXP-4,
              :QGI-PLEXP-5,
              :QGI-DLPARVEX,
              :QGI-PLOFFSET-1,
              :QGI-PLOFFSET-2,
              :QGI-PLOFFSET-3,
              :QGI-PLOFFSET-4,
              :QGI-PLOFFSET-5,
              :QGI-SERVOFF,
              :QGI-PAYCL1,
              :QGI-PAYCL2,
              :QGI-PAYAH,
              :QGI-PAYPP,
              :QGI-PAYO1,
              :QGI-PAYO2,
              :QGI-PAYO3,
              :QGI-PAYO4,
              :QGI-PAYO5,
              :QGI-PAYO6,
              :QGI-PAYO7,
              :QGI-DLPAYABLE,
              :QGI-DLRVPRIN-1,
              :QGI-DLRVPRIN-2,
              :QGI-DLRVPRIN-3,
              :QGI-DLRVCHGS-1,
              :QGI-DLRVCHGS-2,
              :QGI-DLRVCHGS-3,
              :QGI-DLPARV,
              :QGI-PAYALLOT,
              :QGI-DIRECT-PAY,
              :QGI-ERINTCHG,
              :QGI-ERADDON,
              :QGI-ERSERCHG,
              :QGI-ERMAINTFEE,
              :QGI-ERDLDISC,
              :QGI-EROTHER,
              :QGI-EARN7,
              :QGI-EARN8,
              :QGI-EARN9,
              :QGI-EARN10,
              :QGI-LATECHG,
              :QGI-INTEREST,
              :QGI-DEFERMENTS,
              :QGI-SAPRINCIPAL,
              :QGI-PLINT-1,
              :QGI-PLINT-2,
              :QGI-PLINT-3,
              :QGI-PLINT-4,
              :QGI-PLINT-5,
              :QGI-PLREC-1,
              :QGI-PLREC-2,
              :QGI-PLREC-3,
              :QGI-PLREC-4,
              :QGI-PLREC-5,
              :QGI-CL1,
              :QGI-CL2,
              :QGI-AH,
              :QGI-PP,
              :QGI-O1,
              :QGI-O2,
              :QGI-O3,
              :QGI-O4,
              :QGI-O5,
              :QGI-O6,
              :QGI-O7,
              :QGI-PLNEW-AR-1,
              :QGI-PLNEW-AR-2,
              :QGI-PLNEW-AR-3,
              :QGI-PLNEW-AR-4,
              :QGI-PLNEW-AR-5,
              :QGI-REPO-AR,
              :QGI-REPO-INT,
              :QGI-ACR-INT-AR,
              :QGI-CONV-OFFSET,
              :QGI-PARTIAL-PLEXP,
              :QGI-PAYLBOX,
              :QGI-PENALTY-INT,
              :QGI-PAYTPAY,
              :QGI-PAYTCOLL,
              :QGI-PAYMGRAM,
              :QGI-REPO-ADDON-EXP,
              :QGI-ACH-BANK,
              :QGI-LN-APPROVAL-LIMIT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GI1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GI1-FILE.

           ACCEPT GI-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GI-PATH-SQL TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.
           PERFORM SET-GI-FIELDS.

           EXEC SQL
            UPDATE DBO.GIFILE
             SET
              GIFILE.GI_BRANCH = :QGI-BRANCH,
              GIFILE.GI_CLASS = :QGI-CLASS,
              GIFILE.GI_LTOUCH_DATE = :QGI-LTOUCH-DATE,
              GIFILE.GI_CASH = :QGI-CASH,
              GIFILE.GI_BANK = :QGI-BANK,
              GIFILE.GI_NSF = :QGI-NSF,
              GIFILE.GI_AR = :QGI-AR,
              GIFILE.GI_PL_1 = :QGI-PL-1,
              GIFILE.GI_PL_2 = :QGI-PL-2,
              GIFILE.GI_PL_3 = :QGI-PL-3,
              GIFILE.GI_PL_4 = :QGI-PL-4,
              GIFILE.GI_PL_5 = :QGI-PL-5,
              GIFILE.GI_DLINSREB = :QGI-DLINSREB,
              GIFILE.GI_ACR_INT_AR_AR = :QGI-ACR-INT-AR-AR,
              GIFILE.GI_OTHBR_REC = :QGI-OTHBR-REC,
              GIFILE.GI_OTHBR_PAY = :QGI-OTHBR-PAY,
              GIFILE.GI_UNINTCHG = :QGI-UNINTCHG,
              GIFILE.GI_UNADDON = :QGI-UNADDON,
              GIFILE.GI_UNSERCHG = :QGI-UNSERCHG,
              GIFILE.GI_UNMAINTFEE = :QGI-UNMAINTFEE,
              GIFILE.GI_UNDLDISC = :QGI-UNDLDISC,
              GIFILE.GI_UNOTHER = :QGI-UNOTHER,
              GIFILE.GI_UN7 = :QGI-UN7,
              GIFILE.GI_UN8 = :QGI-UN8,
              GIFILE.GI_UN9 = :QGI-UN9,
              GIFILE.GI_UN10 = :QGI-UN10,
              GIFILE.GI_UNCL1 = :QGI-UNCL1,
              GIFILE.GI_UNCL2 = :QGI-UNCL2,
              GIFILE.GI_UNAH = :QGI-UNAH,
              GIFILE.GI_UNPP = :QGI-UNPP,
              GIFILE.GI_UNO1 = :QGI-UNO1,
              GIFILE.GI_UNO2 = :QGI-UNO2,
              GIFILE.GI_UNO3 = :QGI-UNO3,
              GIFILE.GI_UNO4 = :QGI-UNO4,
              GIFILE.GI_UNO5 = :QGI-UNO5,
              GIFILE.GI_UNO6 = :QGI-UNO6,
              GIFILE.GI_UNO7 = :QGI-UNO7,
              GIFILE.GI_UNINTREPO = :QGI-UNINTREPO,
              GIFILE.GI_UNADDONINTREPO = :QGI-UNADDONINTREPO,
              GIFILE.GI_PLEXP_1 = :QGI-PLEXP-1,
              GIFILE.GI_PLEXP_2 = :QGI-PLEXP-2,
              GIFILE.GI_PLEXP_3 = :QGI-PLEXP-3,
              GIFILE.GI_PLEXP_4 = :QGI-PLEXP-4,
              GIFILE.GI_PLEXP_5 = :QGI-PLEXP-5,
              GIFILE.GI_DLPARVEX = :QGI-DLPARVEX,
              GIFILE.GI_PLOFFSET_1 = :QGI-PLOFFSET-1,
              GIFILE.GI_PLOFFSET_2 = :QGI-PLOFFSET-2,
              GIFILE.GI_PLOFFSET_3 = :QGI-PLOFFSET-3,
              GIFILE.GI_PLOFFSET_4 = :QGI-PLOFFSET-4,
              GIFILE.GI_PLOFFSET_5 = :QGI-PLOFFSET-5,
              GIFILE.GI_SERVOFF = :QGI-SERVOFF,
              GIFILE.GI_PAYCL1 = :QGI-PAYCL1,
              GIFILE.GI_PAYCL2 = :QGI-PAYCL2,
              GIFILE.GI_PAYAH = :QGI-PAYAH,
              GIFILE.GI_PAYPP = :QGI-PAYPP,
              GIFILE.GI_PAYO1 = :QGI-PAYO1,
              GIFILE.GI_PAYO2 = :QGI-PAYO2,
              GIFILE.GI_PAYO3 = :QGI-PAYO3,
              GIFILE.GI_PAYO4 = :QGI-PAYO4,
              GIFILE.GI_PAYO5 = :QGI-PAYO5,
              GIFILE.GI_PAYO6 = :QGI-PAYO6,
              GIFILE.GI_PAYO7 = :QGI-PAYO7,
              GIFILE.GI_DLPAYABLE = :QGI-DLPAYABLE,
              GIFILE.GI_DLRVPRIN_1 = :QGI-DLRVPRIN-1,
              GIFILE.GI_DLRVPRIN_2 = :QGI-DLRVPRIN-2,
              GIFILE.GI_DLRVPRIN_3 = :QGI-DLRVPRIN-3,
              GIFILE.GI_DLRVCHGS_1 = :QGI-DLRVCHGS-1,
              GIFILE.GI_DLRVCHGS_2 = :QGI-DLRVCHGS-2,
              GIFILE.GI_DLRVCHGS_3 = :QGI-DLRVCHGS-3,
              GIFILE.GI_DLPARV = :QGI-DLPARV,
              GIFILE.GI_PAYALLOT = :QGI-PAYALLOT,
              GIFILE.GI_DIRECT_PAY = :QGI-DIRECT-PAY,
              GIFILE.GI_ERINTCHG = :QGI-ERINTCHG,
              GIFILE.GI_ERADDON = :QGI-ERADDON,
              GIFILE.GI_ERSERCHG = :QGI-ERSERCHG,
              GIFILE.GI_ERMAINTFEE = :QGI-ERMAINTFEE,
              GIFILE.GI_ERDLDISC = :QGI-ERDLDISC,
              GIFILE.GI_EROTHER = :QGI-EROTHER,
              GIFILE.GI_EARN7 = :QGI-EARN7,
              GIFILE.GI_EARN8 = :QGI-EARN8,
              GIFILE.GI_EARN9 = :QGI-EARN9,
              GIFILE.GI_EARN10 = :QGI-EARN10,
              GIFILE.GI_LATECHG = :QGI-LATECHG,
              GIFILE.GI_INTEREST = :QGI-INTEREST,
              GIFILE.GI_DEFERMENTS = :QGI-DEFERMENTS,
              GIFILE.GI_SAPRINCIPAL = :QGI-SAPRINCIPAL,
              GIFILE.GI_PLINT_1 = :QGI-PLINT-1,
              GIFILE.GI_PLINT_2 = :QGI-PLINT-2,
              GIFILE.GI_PLINT_3 = :QGI-PLINT-3,
              GIFILE.GI_PLINT_4 = :QGI-PLINT-4,
              GIFILE.GI_PLINT_5 = :QGI-PLINT-5,
              GIFILE.GI_PLREC_1 = :QGI-PLREC-1,
              GIFILE.GI_PLREC_2 = :QGI-PLREC-2,
              GIFILE.GI_PLREC_3 = :QGI-PLREC-3,
              GIFILE.GI_PLREC_4 = :QGI-PLREC-4,
              GIFILE.GI_PLREC_5 = :QGI-PLREC-5,
              GIFILE.GI_CL1 = :QGI-CL1,
              GIFILE.GI_CL2 = :QGI-CL2,
              GIFILE.GI_AH = :QGI-AH,
              GIFILE.GI_PP = :QGI-PP,
              GIFILE.GI_O1 = :QGI-O1,
              GIFILE.GI_O2 = :QGI-O2,
              GIFILE.GI_O3 = :QGI-O3,
              GIFILE.GI_O4 = :QGI-O4,
              GIFILE.GI_O5 = :QGI-O5,
              GIFILE.GI_O6 = :QGI-O6,
              GIFILE.GI_O7 = :QGI-O7,
              GIFILE.GI_PLNEW_AR_1 = :QGI-PLNEW-AR-1,
              GIFILE.GI_PLNEW_AR_2 = :QGI-PLNEW-AR-2,
              GIFILE.GI_PLNEW_AR_3 = :QGI-PLNEW-AR-3,
              GIFILE.GI_PLNEW_AR_4 = :QGI-PLNEW-AR-4,
              GIFILE.GI_PLNEW_AR_5 = :QGI-PLNEW-AR-5,
              GIFILE.GI_REPO_AR = :QGI-REPO-AR,
              GIFILE.GI_REPO_INT = :QGI-REPO-INT,
              GIFILE.GI_ACR_INT_AR = :QGI-ACR-INT-AR,
              GIFILE.GI_CONV_OFFSET = :QGI-CONV-OFFSET,
              GIFILE.GI_PARTIAL_PLEXP = :QGI-PARTIAL-PLEXP,
              GIFILE.GI_PAYLBOX = :QGI-PAYLBOX,
              GIFILE.GI_PENALTY_INT = :QGI-PENALTY-INT,
              GIFILE.GI_PAYTPAY = :QGI-PAYTPAY,
              GIFILE.GI_PAYTCOLL = :QGI-PAYTCOLL,
              GIFILE.GI_PAYMGRAM = :QGI-PAYMGRAM,
              GIFILE.GI_REPO_ADDON_EXP = :QGI-REPO-ADDON-EXP,
              GIFILE.GI_ACH_BANK = :QGI-ACH-BANK,
              GIFILE.GI_LN_APPROVAL_LIMIT =
                   :QGI-LN-APPROVAL-LIMIT
             WHERE GIFILE.GI_BRANCH = :QGI-BRANCH
               AND GIFILE.GI_CLASS = :QGI-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GI1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GI1-FILE.

           PERFORM DELETE-IT.
           MOVE GI-PATH-SQL TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.
           PERFORM SET-GI-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GIFILE
             WHERE GIFILE.GI_BRANCH = :QGI-BRANCH
               AND GIFILE.GI_CLASS = :QGI-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GI1-FILE.

      *-----------------------------------------------------------------
       CLOSE-GI1-FILE.

           PERFORM CLOSE-IT.

           IF ( GI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GI1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GI1-CURSOR-STAT.


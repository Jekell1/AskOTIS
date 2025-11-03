      *-----------------------------------------------------------------
       READ-LXG1-FILE.
           PERFORM READ-IT.
           MOVE LXG-PATH-OWNBR  TO LXG-BRNO. 
           MOVE LXG-PATH-SQL    TO E-FILE.
           MOVE LXG1-KEY        TO E-KEYX.
           INITIALIZE QLXG-REC.

           MOVE LXG-BRNO   TO QLXG-BRNO.
           MOVE LXG-ACCTNO TO QLXG-ACCTNO.
           MOVE LXG-SEQNO TO QLXG-SEQNO.

           EXEC SQL
            SELECT LXGFILE.LXG_BRNO,
                   LXGFILE.LXG_ACCTNO,
                   LXGFILE.LXG_SEQNO,
                   CAST(LXGFILE.LXG_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LXGFILE.LXG_GLNO_1,
                   LXGFILE.LXG_GLAMT_1,
                   LXGFILE.LXG_GLNO_2,
                   LXGFILE.LXG_GLAMT_2,
                   LXGFILE.LXG_GLNO_3,
                   LXGFILE.LXG_GLAMT_3
            INTO :QLXG-REC
            FROM DBO.LXGFILE
            WHERE LXGFILE.LXG_BRNO = :QLXG-BRNO
              AND LXGFILE.LXG_ACCTNO = :QLXG-ACCTNO
              AND LXGFILE.LXG_SEQNO = :QLXG-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LXG1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LXG-FIELDS.

      *-----------------------------------------------------------------

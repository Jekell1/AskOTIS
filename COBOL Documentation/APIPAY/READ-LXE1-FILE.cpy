      *-----------------------------------------------------------------
       READ-LXE1-FILE.
           PERFORM READ-IT.
           MOVE LXE-PATH-OWNBR  TO LXE-BRNO. 
           MOVE LXE-PATH-SQL    TO E-FILE.
           MOVE LXE1-KEY        TO E-KEYX.
           INITIALIZE QLXE-REC.

           MOVE LXE-BRNO   TO QLXE-BRNO.
           MOVE LXE-ACCTNO TO QLXE-ACCTNO.
           MOVE LXE-SEQNO TO QLXE-SEQNO.

           EXEC SQL
            SELECT LXEFILE.LXE_BRNO,
                   LXEFILE.LXE_ACCTNO,
                   LXEFILE.LXE_SEQNO,
                   CAST(LXEFILE.LXE_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LXEFILE.LXE_EARN_1,
                   LXEFILE.LXE_EARN_2,
                   LXEFILE.LXE_EARN_3,
                   LXEFILE.LXE_EARN_4,
                   LXEFILE.LXE_EARN_5,
                   LXEFILE.LXE_EARN_6,
                   LXEFILE.LXE_EARN_7,
                   LXEFILE.LXE_WORKER,
                   LXEFILE.LXE_WORKER2
            INTO :QLXE-REC
            FROM DBO.LXEFILE
            WHERE LXEFILE.LXE_BRNO = :QLXE-BRNO
              AND LXEFILE.LXE_ACCTNO = :QLXE-ACCTNO
              AND LXEFILE.LXE_SEQNO = :QLXE-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LXE1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LXE-FIELDS.

      *-----------------------------------------------------------------

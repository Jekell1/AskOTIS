      * COPYMEMBER: LIBLP/LPCDBGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - CDBFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0507 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD CDB-REC FROM QCDB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CDB-FIELDS.
           MOVE QCDB-TYPE TO CDB-TYPE.
           MOVE QCDB-BRANCH TO CDB-BRANCH.
           MOVE QCDB-CODE TO CDB-CODE.

           IF ( QCDB-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CDB-LTOUCH-DATE
           ELSE
              MOVE QCDB-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CDB-LTOUCH-DATE.

           MOVE QCDB-DESC TO CDB-DESC.
           MOVE QCDB-CN-GROUP-ID TO CDB-CN-GROUP-ID.
           MOVE QCDB-CN-PHONE TO CDB-CN-PHONE.
           MOVE QCDB-CN-PHONE-EXT TO CDB-CN-PHONE-EXT.
           MOVE QCDB-GC-GLFEENO TO CDB-GC-GLFEENO.
           MOVE QCDB-GC-GLFEEFINCHG TO CDB-GC-GLFEEFINCHG.
           MOVE QCDB-GC-GLFEE-INT-CHARGABLE
              TO CDB-GC-GLFEE-INT-CHARGABLE.
           MOVE QCDB-GC-FEE-GIFTNET TO CDB-GC-FEE-GIFTNET.
           MOVE QCDB-GC-FEE-PRODNO TO CDB-GC-FEE-PRODNO.
           MOVE QCDB-GC-FEE-PRICE TO CDB-GC-FEE-PRICE.
           MOVE QCDB-GC-NEW-PRODNO TO CDB-GC-NEW-PRODNO.
           MOVE QCDB-GC-FEE-MAXAMT TO CDB-GC-FEE-MAXAMT.
           MOVE QCDB-GC-ALT-GLFEENO TO CDB-GC-ALT-GLFEENO.
           MOVE QCDB-GC-COMM-GLFEENO TO CDB-GC-COMM-GLFEENO.
           MOVE QCDB-GC-COMM-PERCENT TO CDB-GC-COMM-PERCENT.
           MOVE QCDB-GC-FEE-EARNINGS-FG TO CDB-GC-FEE-EARNINGS-FG.
           MOVE QCDB-GC-BRANCH-COMM-AMOUNT
              TO CDB-GC-BRANCH-COMM-AMOUNT.
 
      *-----------------------------------------------------------------
      *    LOAD QCDB-REC FROM CDB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CDB-FIELDS.
           MOVE CDB-TYPE TO QCDB-TYPE.
           MOVE CDB-BRANCH TO QCDB-BRANCH.
           MOVE CDB-CODE TO QCDB-CODE.

           IF ( CDB-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCDB-LTOUCH-DATE
           ELSE
              MOVE CDB-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCDB-LTOUCH-DATE.

           MOVE CDB-DESC TO QCDB-DESC.
           MOVE CDB-CN-GROUP-ID TO QCDB-CN-GROUP-ID.
           MOVE CDB-CN-PHONE TO QCDB-CN-PHONE.
           MOVE CDB-CN-PHONE-EXT TO QCDB-CN-PHONE-EXT.
           MOVE CDB-GC-GLFEENO TO QCDB-GC-GLFEENO.
           MOVE CDB-GC-GLFEEFINCHG TO QCDB-GC-GLFEEFINCHG.
           MOVE CDB-GC-GLFEE-INT-CHARGABLE
            TO QCDB-GC-GLFEE-INT-CHARGABLE.
           MOVE CDB-GC-FEE-GIFTNET TO QCDB-GC-FEE-GIFTNET.
           MOVE CDB-GC-FEE-PRODNO TO QCDB-GC-FEE-PRODNO.
           MOVE CDB-GC-FEE-PRICE TO QCDB-GC-FEE-PRICE.
           MOVE CDB-GC-NEW-PRODNO TO QCDB-GC-NEW-PRODNO.
           MOVE CDB-GC-FEE-MAXAMT TO QCDB-GC-FEE-MAXAMT.
           MOVE CDB-GC-ALT-GLFEENO TO QCDB-GC-ALT-GLFEENO.
           MOVE CDB-GC-COMM-GLFEENO TO QCDB-GC-COMM-GLFEENO.
           MOVE CDB-GC-COMM-PERCENT TO QCDB-GC-COMM-PERCENT.
           MOVE CDB-GC-FEE-EARNINGS-FG TO QCDB-GC-FEE-EARNINGS-FG.
           MOVE CDB-GC-BRANCH-COMM-AMOUNT
            TO QCDB-GC-BRANCH-COMM-AMOUNT.
 

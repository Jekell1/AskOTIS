      * COPYMEMBER: LIBLP/LOADLXA
      ******************************************************************
      *                      LIBLP/LOADLXA
      *
      *   USES LIBLP/LOADLXAW
      *
      *  DESCRIPTION:  READS LX-REC FOR CODES "A" AND "2" AND PUTS THE DATA
      *                IN WLXA-WORKER.
      *
      *  160212 CS  COMBINED 'A' AND '2' RECORDS, THE LXA NOW OCCURS 9
      *             LEFT COPYMEMBER
      *             SAVED VERSION LOADLXA.160210
      * BAH 190116 GLOBAL LXFILE, L2G, SPLIT
      ******************************************************************
       GET-LXA-FILE SECTION.
           PERFORM INITIALIZE-WLXA-WORKER.
           MOVE " " TO WLXA-STATUS.

      * READ ADDON REC.
           MOVE LP-BRNO   TO LX-BRNO.
           MOVE LP-ACCTNO TO LX-ACCTNO.
           MOVE LP-SEQNO  TO LX-SEQNO.
           PERFORM READ-LX1-FILE.
      * LXA-CODE WILL BE AN "A" IF ANY ADDON INFO WAS WRITTEN TO THE
      * NEWLY COMBINED LXFILE LAYOUT
           IF (IO-FG NOT = 0) OR (LXA-CODE NOT = "A")
              MOVE "N" TO WLXA-STATUS
              GO TO EXIT-GET-LXA-FILE.

      *    ---------------------------------------
      *    LOAD TABLE WLXA TABLE ENTRIES 1 THRU 9.
      *    ---------------------------------------
           PERFORM VARYING LXASUB FROM 1 BY 1 UNTIL LXASUB > 9
              MOVE LXA-START-PMT(LXASUB)  TO WLXA-START-PMT(LXASUB)
              MOVE LXA-END-PMT(LXASUB)    TO WLXA-END-PMT(LXASUB)
              MOVE LXA-NET-CHANGE(LXASUB) TO WLXA-NET-CHANGE(LXASUB)
           END-PERFORM.

      *    ---------------------------------------
      *    SAVE REST OF LXA-REC
      *    ---------------------------------------
           MOVE LXA-SRCD          TO WLXA-SRCD.
           MOVE LXA-LN-INSTBL     TO WLXA-LN-INSTBL.
           MOVE LXA-LN-INS-REBATE TO WLXA-LN-INS-REBATE.
           MOVE LXA-LN-INSCODES   TO WLXA-LN-INSCODES.

       EXIT-GET-LXA-FILE.
           EXIT.

      ******************************************************************
      *
      *   USES LIBLP/LOADLXAW
      *
      *  DESCRIPTION:  SAVES THE DATA IN WLXA WORKER IN THE LXA FILE.
      *
      *
      ******************************************************************
       SAVE-WLXA-WORKER SECTION.

           MOVE LP-BRNO   TO LX-BRNO.
           MOVE LP-ACCTNO TO LX-ACCTNO.
           MOVE LP-SEQNO  TO LX-SEQNO.
           PERFORM READ-LX1-FILE.
           IF IO-FG = 0
              PERFORM MOVE-LXA-FIELDS
              PERFORM REWRITE-LX1-FILE
           ELSE
              PERFORM CLEAR-LX-FILE
              MOVE LP-BRNO   TO LX-BRNO
              MOVE LP-ACCTNO TO LX-ACCTNO
              MOVE LP-SEQNO  TO LX-SEQNO
              PERFORM MOVE-LXA-FIELDS
              PERFORM WRITE-LX1-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       EXIT-SAVE-WLXA-WORKER.
           EXIT.

      ********************************************
       MOVE-LXA-FIELDS SECTION.
      ********************************************
            
           MOVE "A" TO LXA-CODE.

      * LOAD TABLE WLXA TABLE ENTRIES 1 THRU 9.

           PERFORM VARYING LXASUB FROM 1 BY 1 UNTIL LXASUB > 9
              MOVE WLXA-START-PMT(LXASUB ) TO LXA-START-PMT(LXASUB)
              MOVE WLXA-END-PMT(LXASUB)    TO LXA-END-PMT(LXASUB)
              MOVE WLXA-NET-CHANGE(LXASUB) TO LXA-NET-CHANGE(LXASUB)
           END-PERFORM.

           MOVE WLXA-SRCD          TO LXA-SRCD.
           MOVE WLXA-LN-INSTBL     TO LXA-LN-INSTBL.
           MOVE WLXA-LN-INS-REBATE TO LXA-LN-INS-REBATE.
           MOVE WLXA-LN-INSCODES   TO LXA-LN-INSCODES.



      ******************************************
       INITIALIZE-WLXA-WORKER SECTION.
      ******************************************
           MOVE SPACES TO WLXA-SRCD
                          WLXA-LN-INS-REBATE
                          WLXA-LN-INSCODES.
           PERFORM VARYING LXASUB FROM 1 BY 1 UNTIL LXASUB > 9
              MOVE 0 TO WLXA-START-PMT(LXASUB)
                        WLXA-END-PMT(LXASUB)
                        WLXA-NET-CHANGE(LXASUB)
           END-PERFORM.
           MOVE 0 TO WLXA-LN-INSCOMP
                     WLXA-LN-INSOURS
                     WLXA-LN-INSEFF-DATE
                     WLXA-LN-INSEXP-DATE
                     WLXA-LN-INSPREM
                     WLXA-LN-INSCOVR
                     WLXA-LN-INSCOMM.

       EXIT-INITIALIZE-WLXA-WORKER.
           EXIT.

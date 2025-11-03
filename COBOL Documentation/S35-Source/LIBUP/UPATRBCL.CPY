      * COPYMEMBER: LIBLP/LPATRBCL
      * BAH 20210423 REMOVED FUTURE FIELDS


      ***************************************************
       CLEAR-ATRB-FILE SECTION.
      ***************************************************
           MOVE ALL X"09" TO ATRB-REC.

           MOVE 0 TO ATB-DATE ATB-BRNO ATB-CLASS ATB-ACCTNO 
                     ATB-LAWCODE ATB-LINE ATB-LOANDATE ATB-LTOUCH-DATE
                     ATB-PROCEEDS ATB-LNAMT ATB-FINCHG ATB-INTCHG
                     ATB-SERCHG ATB-EXTCHG ATB-MAINTFEE ATB-ORGTERM
                     ATB-DLNO ATB-DLRV ATB-DLSPRV ATB-DLDISC
                     ATB-DLPARV ATB-DLPARTRCAMT ATB-PURBAL
                     ATB-REF-ACCTNO ATB-DLPARVPAID ATB-DLPARVHELD
                     ATB-POSTDATE ATB-POSTTIME ATB-REFBR.

           MOVE 0 TO ATB-SUBCLASS.

           MOVE SPACES TO ATB-REF-FRTO ATB-PURCD ATB-SRCD ATB-COLLCD
                          ATB-SERVACCT ATB-LOANTYPE ATB-INSURED
                          ATB-DLRECOURSE ATB-USERID ATB-PLCD
                          ATB-PLTYPE ATB-REPOCD.

           PERFORM CLEAR-ATB-TABLES VARYING TRBSUB FROM 1 BY 1
                                    UNTIL TRBSUB > 10.

           GO TO EXIT-CLEAR-ATRB-FILE.

       CLEAR-ATB-TABLES.
           IF TRBSUB < 4
              MOVE 0 TO ATB-DLRVPRIN(TRBSUB) ATB-DLRVCHGS(TRBSUB).

           IF TRBSUB < 5
              MOVE 0      TO ATB-SSNO(TRBSUB)
              MOVE SPACES TO ATB-MAKERCD(TRBSUB).


           MOVE 0 TO ATB-UNEARN(TRBSUB)
                     ATB-INSUNEARN(TRBSUB).

       EXIT-CLEAR-ATRB-FILE.
           EXIT.

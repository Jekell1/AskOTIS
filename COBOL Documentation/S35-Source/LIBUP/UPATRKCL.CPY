      * COPYMEMBER: LIBUP/UPTRKCL

      **************************************************************************
      *       ASCII POSTING TRANSACTION FILE CLEAR ROUTINE
      *                   CONVERSION
      *
      **************************************************************************
       CLEAR-ATRK-FILE SECTION.
           MOVE ALL X"09" TO ATRK-REC.

           MOVE 0 TO ATK-DATE ATK-BRNO ATK-CLASS ATK-ACCTNO
                     ATK-LAWCODE ATK-LINE ATK-LTOUCH-DATE ATK-REFBR
                     ATK-LOANDATE ATK-PROCEEDS ATK-LNAMT ATK-FINCHG
                     ATK-INTCHG ATK-SERCHG ATK-EXTCHG ATK-MAINTFEE
                     ATK-ORGTERM ATK-DLNO ATK-DLRV ATK-DLSPRV
                     ATK-DLDISC ATK-DLPARV ATK-DLPARTRCAMT ATK-PURBAL
                     ATK-PRLNNO ATK-DLPARVPAID ATK-DLPARVHELD
                     ATK-POSTDATE ATK-POSTTIME ATK-SUBCLASS.

           PERFORM CLEAR-ATK-TABLES VARYING TRKSUB FROM 1 BY 1
                                    UNTIL TRKSUB > 10.

           MOVE SPACES TO ATK-REF-FRTO ATK-PURCD ATK-SRCD ATK-COLLCD
                          ATK-SERVACCT ATK-LOANTYPE ATK-REPOCD
                          ATK-PLCD ATK-PLTYPE ATK-INSURED
                          ATK-DLRECOURSE ATK-USERID.

      ******************************************
       CLEAR-ATK-TABLES SECTION.
      ******************************************
           IF TRKSUB < 4
              MOVE 0 TO ATK-DLRVPRIN(TRKSUB) ATK-DLRVCHGS(TRKSUB).
           IF TRKSUB < 5
              MOVE 0 TO ATK-SSNO(TRKSUB)
              MOVE SPACES TO ATK-MAKERCD(TRKSUB).

           MOVE 0 TO ATK-UNEARN(TRKSUB)
                     ATK-EARNED(TRKSUB)
                     ATK-INSEARNED(TRKSUB)
                     ATK-INSUNEARN(TRKSUB).


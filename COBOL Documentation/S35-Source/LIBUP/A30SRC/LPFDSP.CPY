      * COPYMEMBER: LIBLP/LPFDSP
      **************************************************
      *            ( STATE PARAMETER FILE )
      **************************************************
       FD  OSP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01SP.CPY" REPLACING LEADING "SP" BY "OSP"
                                         LEADING "EXPR" BY "OEXPR"
                                         LEADING "ASTP" BY "OASTP"
                                         LEADING "CTBL" BY "OCTBL"
                                         LEADING "CSTP" BY "OCSTP"
                                         LEADING "ETBL" BY "OETBL"
                                         LEADING "ESTP" BY "OESTP"
                                         LEADING "ILEV" BY "OILEV"
                                         LEADING "ISTP" BY "OISTP"
                                         LEADING "IVER" BY "OIVER".

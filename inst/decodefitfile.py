# -*- coding: utf-8 -*-

import fitdecode
import sys
import pandas as pd

src_file = "c:\\test\\2019-11-09-12-30-38.fit"


def message_df(fitfile=None,
               msgtype='record',
               outfile=None,
               appendunits=True,
               missing='drop',
               addlasttimestamp=False,
               fromR=False)  :

    if fitfile is None:
        print ("No fitfile given")
        sys.exit(1)

    lasttimestamp = pd.to_datetime(float("NaN"))
    msgdf =  pd.DataFrame()
    
    with fitdecode.FitReader(fitfile) as fit:
        for frame in fit:
            # The yielded frame object is of one of the following types:
            # * fitdecode.FitHeader
            # * fitdecode.FitDefinitionMessage
            # * fitdecode.FitDataMessage
            # * fitdecode.FitCRC
            if isinstance(frame, fitdecode.FitDataMessage):
                # Here, frame is a FitDataMessage object.
                # A FitDataMessage object contains decoded values that
                # are directly usable in your script logic.
                if frame.has_field('timestamp'):
                    lasttimestamp = frame.get_value('timestamp')
                if frame.name == msgtype:
                    msgdict = {}
                    if addlasttimestamp and not frame.has_field('timestamp'):
                        msgdict['timestamp'] = lasttimestamp
                    # Go through all the data entries in this msg
                    for fld in frame.fields:
                        if fld.units and appendunits:
                            keyname = fld.name + "." + fld.units.replace("/",".")
                        else:
                            keyname = fld.name
 
                        msgdict[keyname] = frame.get_value(fld.name,fallback=float('NaN'))

                    msgdf = msgdf.append(msgdict,ignore_index=True)

    msgdf = msgdf.where((pd.notnull(msgdf)), None)
    if missing == 'drop':
        msgdf.dropna(axis=1,how='all',inplace=True)
                        
                
    if not fromR :
        print("variables extracted:")
        print("\n".join(str(x) for x in msgdf.columns))
        print("dtypes: ")
        print(msgdf.dtypes)

    if outfile is None :
        return msgdf
    else:
         msgdf.to_json(path_or_buf=outfile,date_format='iso',
                       date_unit='s')
                   

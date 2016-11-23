package com.hpe.adm.octane.ideplugins.services.util;

import com.hpe.adm.nga.sdk.exception.NgaException;
import com.hpe.adm.nga.sdk.model.StringFieldModel;
import org.json.JSONException;
import org.json.JSONObject;

public class SdkUtil {

    /**
     * Attempt to get the error message from and NgaException, if it fails returns null instead
     * @param ex
     * @return
     */
    public static String getMessageFromNgaException(NgaException ex){
        JSONObject object = getJsonFromNgaException(ex);
        if(object != null){
            return object.getString("description");
        } else {
            return null;
        }
    }

    public static JSONObject getJsonFromNgaException(NgaException ex){
        //TODO: wish the NgaException would give more structured details...

        String jsonMessage;
        try{
            //Description: private constant in NgaException.errorModel...
            jsonMessage = ((StringFieldModel)ex.getError().getValue("Description")).getValue();
        } catch (ClassCastException | NullPointerException castException){
            return null;
        }

        int firstIndex = jsonMessage.indexOf("{");
        if(firstIndex != -1){
            jsonMessage = jsonMessage.substring(firstIndex);
        } else {
            return null;
        }

        //Attempt to parse the json message
        try {
            JSONObject jsonObject = new JSONObject(jsonMessage);
            return jsonObject;
        } catch(JSONException jsonException){
           return null;
        }
    }

}

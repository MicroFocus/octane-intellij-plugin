package com.hpe.adm.octane.ideplugins.services.util;

import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.model.StringFieldModel;
import org.json.JSONException;
import org.json.JSONObject;

public class SdkUtil {

    /**
     * Attempt to get the error message from and OctaneException, if it fails returns null instead
     * @param ex
     * @return
     */
    public static String getMessageFromOctaneException(OctaneException ex){
        JSONObject object = getJsonFromOctaneException(ex);
        if(object != null){
            return object.getString("description");
        } else {
            return null;
        }
    }

    public static JSONObject getJsonFromOctaneException(OctaneException ex){
        //TODO: wish the OctaneException would give more structured details...

        String jsonMessage;
        try{
            //Description: private constant in OctaneException.errorModel...
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

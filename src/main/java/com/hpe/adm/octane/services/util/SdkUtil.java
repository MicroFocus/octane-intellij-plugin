package com.hpe.adm.octane.services.util;

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
        String message = getStringFromErrorModel(ex);
        if(message != null){

            //Try to get the json version
            JSONObject jsonMessage = getJsonFromOctaneException(message);
            if(jsonMessage != null){
                return jsonMessage.getString("description");
            }

            //otherwise the plain message is good enough
            return message;
        }

        //Well, I'm sorry
        return null;
    }

    public static JSONObject getJsonFromOctaneException(String message){
        int firstIndex = message.indexOf("{");
        if(firstIndex != -1){
            message = message.substring(firstIndex);
        } else {
            return null;
        }

        //Attempt to parse the json message
        try {
            JSONObject jsonObject = new JSONObject(message);
            return jsonObject;
        } catch(JSONException jsonException){
           return null;
        }
    }

    private static String getStringFromErrorModel(OctaneException ex){
        //TODO: wish the OctaneException would give more structured details...
        try{
            //Description: private constant in OctaneException.errorModel...
            return ((StringFieldModel)ex.getError().getValue("Description")).getValue();
        } catch (ClassCastException | NullPointerException castException){
            return null;
        }
    }

}

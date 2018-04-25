//package com.hpe.adm.octane.ideplugins.intellij.ui.util;
//
//import javax.swing.*;
//import java.awt.event.*;
//
//public class TruncatingTextField extends JTextField {
//
//    private String originalText = "";
//
//    public TruncatingTextField(){
//        super();
//        addComponentListener(new ComponentListener() {
//            @Override
//            public void componentResized(ComponentEvent e) {
//                double containerWidth = getSize().getWidth();
//                int stringWidth = getTextWidth();
//
//                if (stringWidth > containerWidth) {
//                    setInternalText(getTrunctated(originalText, containerWidth));
//                } else {
//                    setInternalText(originalText);
//                }
//            }
//        });
//    }
//
//    public int getTextWidth(){
//        return
//    }
//}

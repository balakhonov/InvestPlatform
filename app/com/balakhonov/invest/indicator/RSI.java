package com.balakhonov.invest.indicator;

import java.util.List;

public class RSI {

    public static Double calculate(List<Double> data, Integer periodLength) throws Exception {
        int lastBar = data.size() - 1;
        int firstBar = lastBar - periodLength + 1;
        if (firstBar < 0) {
            String msg = "Quote history length " + data.size() + " is insufficient to calculate the indicator.";
            throw new Exception(msg);
        }

        double aveGain = 0, aveLoss = 0;
        for (int bar = firstBar + 1; bar <= lastBar; bar++) {
            double change = data.get(bar) - data.get(bar - 1);
            if (change >= 0) {
                aveGain += change;
            } else {
                aveLoss += change;
            }
        }

        double rs = aveGain / Math.abs(aveLoss);
        double rsi = 100 - 100 / (1 + rs);

//        System.out.println("RSI: " + rsi);
        return rsi;
    }

}

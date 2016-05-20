/**
 * @author Adrian Rodriguez Bazaga, Eleazar Diaz Delgado
 * @version 1.0.0
 * @date 19 May 2016
 * @email Adri�n: alu0100826456@ull.edu.es / arodriba@ull.edu.es | Eleazar: eleazardzdo@gmail.com
 * @subject Inteligencia Artificial Avanzada (Advanced Artificial Intelligence)
 * @title Assignment 6 - Classification using Natural Language Processing
 */

package helpers;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

import javafx.util.Pair;

public class FileParser {  
  public static ArrayList<String> parseCorpus(String fileName) {
    try(BufferedReader br = new BufferedReader(new InputStreamReader(FileParser.class.getResourceAsStream(fileName)))) {
      ArrayList<String> listToReturn = new ArrayList<String>();
      for(String line; (line = br.readLine()) != null; ) {
        listToReturn.add(removeUnwantedText(line));
      }
      return listToReturn;
    } catch (IOException e) {
      System.out.println(e.getMessage());
    }
    return null;
  }
  
  private static String removeUnwantedText(String initialText) {
    String textToReturn = "";
    String[] parts = initialText.split(" ");    // Split by spaces or ...
    
    // Remove unwanted parts
    for(String word : parts) {
      if(word.startsWith("Texto:") || word.startsWith("(Vine") || word.startsWith("via") || word.startsWith("@") || word.startsWith(")")) {
        word = "";
        if(word.endsWith(" ")) {
          word = word.replace(word.substring(word.length()-1), "");
        }
      }
      // Add it to the text to return
      if(word != "") {
        textToReturn += word.toLowerCase() + " ";
      }
    }
    
    return textToReturn;
  }
  
  private static Pair<String, Double> getProbPair(String initialText) {
    Pair<String, Double> pairToReturn;
    String[] parts = initialText.split(" |:");
    
    parts[0] = parts[1];
    parts[1] = parts[5];
    
    //System.out.println("Palabra: " + parts[0] + " Probabilidad: " + parts[1]);
    
    pairToReturn = new Pair<String, Double>((String)parts[0], new Double(parts[1]));
   
    return pairToReturn;
  }
  
  public static ArrayList<Pair<String, Double>> parseProbFile(String fileName) {
    try(BufferedReader br = new BufferedReader(new InputStreamReader(FileParser.class.getResourceAsStream(fileName)))) {
      int lineCount = 0;
      ArrayList<Pair<String, Double>> arrayToReturn = new ArrayList<Pair<String, Double>>();
      for(String line; (line = br.readLine()) != null; ) {
        lineCount++;
        if(lineCount > 2) {
          arrayToReturn.add(getProbPair(line));
        }
      }
      return arrayToReturn;
    } catch (IOException e) {
      System.out.println(e.getMessage());
    }    
    return null;
  }
}

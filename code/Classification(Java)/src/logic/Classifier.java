/**
 * @author Adrian Rodriguez Bazaga, Eleazar Diaz Delgado
 * @version 1.0.0
 * @date 19 May 2016
 * @email Adri�n: alu0100826456@ull.edu.es / arodriba@ull.edu.es | Eleazar: eleazardzdo@gmail.com
 * @subject Inteligencia Artificial Avanzada (Advanced Artificial Intelligence)
 * @title Assignment 6 - Classification using Natural Language Processing
 */

package logic;

import helpers.FileParser;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javafx.util.Pair;

public class Classifier {
  private ArrayList<String> corpus;
  List<String> uniqueCorpus; 
  private Map<String, Float> relWordProbs = new TreeMap<String, Float>();
  private Map<String, Float> notRelWordProbs = new TreeMap<String, Float>();

  public Classifier() {   
    setCorpus(new ArrayList<String>());
  }

  public ArrayList<String> getCorpus() {
    return corpus;
  }

  public void setCorpus(ArrayList<String> corpus) {
    this.corpus = corpus;
  }

  public void createUniqueCorpus() {
    Map<String, String> uniqueCorpusMap = new TreeMap<String, String>();
    for(String line : corpus) {
      uniqueCorpusMap.put(line, line);
    }
    uniqueCorpus = new ArrayList<>(uniqueCorpusMap.values()); 
  }

  public void showUniqueCorpus() {
    for (String temp : getUniqueCorpus()) {
      System.out.println("Texto:" + temp);
    }
  }

  public void classifyText() {
    Float relProbAccum = new Float(0);
    Float nRelProbAccum = new Float(0);
    int relProbCount = 0;
    int nRelProbCount = 0;
    BufferedWriter bw = null;        // Buffered writer used for the output file
    
    // Remove two first elements of the corpus (they're just info, not text)
    List<String> usedCorpus = getUniqueCorpus();
    usedCorpus.remove(0);
    usedCorpus.remove(0);

    try {
      File fout = new File("generated/clasificacion.txt");
      FileOutputStream fos = new FileOutputStream(fout);
      bw = new BufferedWriter(new OutputStreamWriter(fos));

      for (String temp : usedCorpus) {
        String[] textWords = temp.split(" ");
        for(String word : textWords) {
          // Relevant probability
          Float probability = getRelWordProbs().get(word);
          if(probability != null) {
            relProbAccum += probability;
          }
          // Not relevant probability
          probability = getNotRelWordProbs().get(word);
          if(probability != null) {
            nRelProbAccum += probability;
          }
          //System.out.println("?1: " + relProbAccum);
          //System.out.println("?2: " + nRelProbAccum);
        }
        try {
          if(relProbAccum > nRelProbAccum) {
            relProbCount++;
            bw.write("Clase:rel" + "\t\t" + "Texto:" + temp);
            bw.newLine();
          } else {
            nRelProbCount++;
            bw.write("Clase:nrel" + "\t" + "Texto:" + temp);
            bw.newLine();
          }
        } catch (IOException e) {
          System.out.println(e.getMessage());
        }
        // Reset for next iteration
        relProbAccum = new Float(0);
        nRelProbAccum = new Float(0);
      }

      // Close the output file
      bw.close();
    } catch (FileNotFoundException e) {
      System.out.println(e.getMessage());
    } catch (IOException e) {
      System.out.println(e.getMessage());
    }

    System.out.println("Textos clasificados como relevantes: " + relProbCount);
    System.out.println("Textos clasfiicados como no relevantes: " + nRelProbCount);
  }

  public void showCorpus() {
    for(String text : getCorpus()) {
      System.out.println(text);
    }
  }

  public int getUniqueCorpusSize() {
    return getUniqueCorpus().size();
  }

  public int getCorpusSize() {
    return getCorpus().size();
  }

  public List<String> getUniqueCorpus() {
    return uniqueCorpus;
  }

  public void setUniqueCorpus(List<String> uniqueCorpus) {
    this.uniqueCorpus = uniqueCorpus;
  }

  public void getRelProbOfWords() {
    ArrayList<Pair<String, Float>> pairs = FileParser.parseProbFile("corpusrel.txtlearn");
    for(Pair<String,Float> pair : pairs) {
      getRelWordProbs().put(pair.getKey(), pair.getValue());
    }
    //System.out.println("Probabilidades relevantes: " + getRelWordProbs().size());
  }

  public void getNrelProbOfWords() {
    ArrayList<Pair<String, Float>> pairs = FileParser.parseProbFile("corpusnrel.txtlearn");
    for(Pair<String,Float> pair : pairs) {
      getNotRelWordProbs().put(pair.getKey(), pair.getValue());
    }
    //System.out.println("Probabilidades no relevantes: " + getNotRelWordProbs().size());
  }

  public Map<String, Float> getRelWordProbs() {
    return relWordProbs;
  }

  public void setRelWordProbs(Map<String, Float> relWordProbs) {
    this.relWordProbs = relWordProbs;
  }

  public Map<String, Float> getNotRelWordProbs() {
    return notRelWordProbs;
  }

  public void setNotRelWordProbs(Map<String, Float> notRelWordProbs) {
    this.notRelWordProbs = notRelWordProbs;
  }
}

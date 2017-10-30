#include <Rcpp.h>
using namespace Rcpp;
#include <cytolib/global.hpp>

#ifdef PRT
 bool isOptimum = true;
#include <cytolib/MemCytoFrame.hpp>
typedef vector<pair<string, string>> kw_type;
// typedef MemCytoFrame<vec_kw_constainer> MyMemCytoFrame;
// typedef unordered_map<string, string> kw_type;
// typedef MemCytoFrame<kw_type> MyMemCytoFrame;
typedef MemCytoFrame MyMemCytoFrame;

// [[Rcpp::export]] 
void writeH5(Rcpp::XPtr<MyMemCytoFrame> fr, string filename){
  fr->writeH5(filename);
  
}
                                      

// [[Rcpp::export]] 
Rcpp::XPtr<MyMemCytoFrame> parseFCS(string filename, vector<int> which_lines
                                      ,string transformation="linearize",
                      float decades=0,
                      bool truncate_min_val = false,
                      float min_limit=-111,
                      bool truncate_max_range = true,
                      int dataset = 1,
                      bool emptyValue= true,
                      bool ignoreTextOffset = true,
                      bool onlyTxt = false,
                      int num_threads = 1)
{

  	FCS_READ_PARAM config;
    config.header.ignoreTextOffset = ignoreTextOffset;
    config.header.nDataset = dataset;
    config.header.isEmptyKeyValue = emptyValue;

    config.data.which_lines = which_lines;
    config.data.decades = decades;
    config.data.truncate_min_val = truncate_min_val;
    config.data.min_limit = min_limit;
    config.data.truncate_max_range = truncate_max_range;
    config.data.num_threads = num_threads;
    if(transformation=="linearize")
      config.data.transform = TransformType::linearize;
    else if(transformation=="none")
      config.data.transform = TransformType::none;
    else if(transformation=="linearize_with_PnG_scaling")
      config.data.transform = TransformType::linearize_with_PnG_scaling;
    else if(transformation=="scale")
      config.data.transform = TransformType::scale;
    else
      stop("unkown transformation type :" + transformation);

    return Rcpp::XPtr<MyMemCytoFrame>(new MyMemCytoFrame(filename.c_str(), config, onlyTxt));

}

// [[Rcpp::export]] 
NumericVector getData(Rcpp::XPtr<MyMemCytoFrame> fr){
  int nrow = fr->nRow();
  int ncol = fr->nCol();
  int ntotal = ncol * nrow;
  
  float * dat = fr->getData();
  NumericVector mat(dat, dat + ntotal);
  mat.attr("dim") = Dimension(nrow, ncol);
  StringVector chnl = wrap(fr->getChannels());
  StringVector cid(ncol);
  for(int i = 0; i < ncol; i++)
    cid[i] = "$P" + to_string(i+1) + "N";
  
    
  chnl.attr("names") = cid;
  mat.attr("dimnames") = List::create(R_NilValue, chnl);
  return mat;
}

// [[Rcpp::export]] 
string getKeyword(Rcpp::XPtr<MyMemCytoFrame> fr, string key){
  
  string res = fr->getKeyword(key);
  return res;
}

// [[Rcpp::export]] 
kw_type getKeywords(Rcpp::XPtr<MyMemCytoFrame> fr){
  // return fr->getKeywords().getPairs();
  return fr->getKeywords();
}

// [[Rcpp::export]] 
int getncol(Rcpp::XPtr<MyMemCytoFrame> fr){
  
  return fr->nCol();
}

// [[Rcpp::export]] 
int getnrow(Rcpp::XPtr<MyMemCytoFrame> fr){
  
  return fr->nRow();
}

// [[Rcpp::export]] 
Rcpp::DataFrame getpdata(Rcpp::XPtr<MyMemCytoFrame> fr){
  
  int ncol = fr->nCol();
  StringVector rowid(ncol);
  StringVector names(ncol);
  StringVector desc(ncol);
  NumericVector range(ncol);
  NumericVector minRange(ncol);
  NumericVector maxRange(ncol);
  vector<string> chnl = fr->getChannels();
  vector<string> marker = fr->getMarkers();
  for(int i = 0; i < ncol; i++)
  {
    rowid[i] = "$P" + to_string(i+1);
    names[i] = chnl[i];
    if(marker[i].empty())
      desc[i] = StringVector::get_na();
    else
      desc[i] = marker[i];
    pair<float, float> r = fr->getRange(chnl[i], ColType::channel, RangeType::instrument);
    maxRange[i] = range[i] = r.second;
    minRange[i] = r.first;
  }
  rowid.attr("class") = "AsIs";
  desc.attr("class") = "AsIs";
  names.attr("class") = "AsIs";
  DataFrame df = DataFrame::create(Named("name") = names
                             ,Named("desc") = desc
                             ,Named("range") = range
                             ,Named("minRange") = minRange
                             ,Named("maxRange") = maxRange
                             );
  df.attr("row.names") = rowid;
  return df;
}
#else
bool isOptimum = false;
#endif

// [[Rcpp::export]]
bool isCytoLibOptimum(){return isOptimum;}

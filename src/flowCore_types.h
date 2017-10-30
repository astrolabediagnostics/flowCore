/*
 * flowCore_types.h
 *
 *  Created on: Nov 24, 2015
 *      Author: wjiang2
 */

#ifndef FLOWCORE_TYPES_H_
#define FLOWCORE_TYPES_H_


#include "pairVectorRcppWrap.h"
#include "convertRawBytes.h"
#include <cytolib/global.hpp>
#ifdef PRT
#include <cytolib/MemCytoFrame.hpp>
typedef vector<pair<string, string>> kw_type;
// typedef MemCytoFrame<vec_kw_constainer> MyMemCytoFrame;
// typedef unordered_map<string, string> kw_type;
// typedef MemCytoFrame<kw_type> MyMemCytoFrame;
typedef MemCytoFrame MyMemCytoFrame;
#endif

#endif /* FLOWCORE_TYPES_H_ */

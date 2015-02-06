#pragma once

#include <string>
#include <vector>
#include <map>
#include <stdexcept>

namespace term {


enum TermType {
	TTNil,
	TTOrdinalInt,
	TTNominalInt,
	TTSymbol,
	TTTuple,
	TTList,
	TTRecord,
	TTChoice,
	TTUpVar,
	TTDownVar,
	TTEof
};

struct Term {
	enum TermType ttType;
	Term(enum TermType tt) : ttType(tt) {}
	Term() : ttType(TTEof) {}
};

std::string toString(const std::unique_ptr<Term> &term, bool escape_quotes = false);

struct TermComparator {
    bool operator() (const std::pair<std::unique_ptr<term::Term>, std::unique_ptr<term::Term>>&,
                     const std::pair<std::unique_ptr<term::Term>, std::unique_ptr<term::Term>>&) const;
};

struct Nil : Term {};

struct OrdinalInt : Term {
	int value;
	explicit OrdinalInt(int v) : value(v), Term(TTOrdinalInt) {}
};

struct NominalInt : Term {
	int value;
	explicit NominalInt(int v) : value(v), Term(TTNominalInt) {}
};

struct Symbol : Term {
	std::string value;
	explicit Symbol(const std::string &s) : value(s), Term(TTSymbol) {}
};

struct Tuple : Term {
	std::vector<std::unique_ptr<Term>> value;
	explicit Tuple(std::vector<std::unique_ptr<Term>> &v) : Term(TTTuple) {
		std::move(v.begin(), v.end(), std::back_inserter(value));
	}
	explicit Tuple(const std::initializer_list<Term> &il) : Term(TTTuple) {
		for (const Term &el : il) {
			value.push_back(std::unique_ptr<Term>(new Term(el)));
		}
	}
};

struct List : Term {
	std::vector<std::unique_ptr<Term>> head;
	std::string tail;
	List(std::vector<std::unique_ptr<Term>> &h, const std::string &t) : tail(t), Term(TTList) {
		std::move(h.begin(), h.end(), std::back_inserter(head));
	}
};

struct Record : Term {
	std::map<std::string, std::unique_ptr<Term>> head;
	std::string tail;
	Record(std::map<std::string, std::unique_ptr<Term>> &h, const std::string &t) : tail(t), Term(TTRecord) {
		std::move(h.begin(), h.end(), std::inserter(head, head.end()));
	}

	Record(std::map<std::string, std::unique_ptr<Term>> &h) : tail(""), Term(TTRecord) {
		std::move(h.begin(), h.end(), std::inserter(head, head.end()));
	}
};

struct Choice : Term {
	std::map<std::string, std::unique_ptr<Term>> head;
	std::string tail;
	Choice(std::map<std::string, std::unique_ptr<Term>> &h, const std::string &t) : tail(t), Term(TTChoice) {
		std::move(h.begin(), h.end(), std::inserter(head, head.end()));
	}

	Choice(std::map<std::string, std::unique_ptr<Term>> &h) : tail(""), Term(TTChoice) {
		std::move(h.begin(), h.end(), std::inserter(head, head.end()));
	}
};

struct UpVar : Term {
	std::string value;
	explicit UpVar(const std::string &s) : value(s), Term(TTUpVar) {}
};

struct DownVar : Term {
	std::string value;
	explicit DownVar(const std::string &s) : value(s), Term(TTDownVar) {}
};

inline std::unique_ptr<Term> make_symbol(const std::string &s) {
	return std::unique_ptr<Term>(new Symbol(s));
}

inline std::unique_ptr<Term> make_var(const std::string &s) {
    if (s.find("_UP_") != std::string::npos)
        return std::unique_ptr<Term>(new UpVar(s));
    else
        return std::unique_ptr<Term>(new DownVar(s));
}


}

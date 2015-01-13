#include "CalTerm.h"

using namespace term;

std::string term::toString(const std::unique_ptr<term::Term> &t) {
    using namespace term;
    switch (t->ttType) {
        case TTNil:
            return "nil";
            break;
        case TTOrdinalInt: {
            const OrdinalInt* casted = static_cast<OrdinalInt*>(t.get());
            return "~" + std::to_string(casted->value);
            break;
        }
        case TTNominalInt: {
            const NominalInt* casted = static_cast<NominalInt*>(t.get());
            return std::to_string(casted->value);
            break;
        }
        case TTSymbol: {
            const Symbol* casted = static_cast<Symbol*>(t.get());
            return "\"" + casted->value + "\"";
            break;
        }
        case TTTuple: {
            const Tuple* casted = static_cast<Tuple*>(t.get());
            std::string ret = "(";
            for (auto it = casted->value.begin(); it != casted->value.end(); ++it) {
                if (it != casted->value.begin())
                    ret += " ";
                ret += toString(*it);
            }
            ret += ")";
            return ret;
            break;
        }
        case TTList: {
            const List* casted = static_cast<List*>(t.get());
            std::string ret = "[";
            for (auto it = casted->head.begin(); it != casted->head.end(); ++it) {
                if (it != casted->head.begin())
                    ret += ", ";
                ret += toString(*it);
            }
            if (!casted->tail.empty())
                ret += "| $" + casted->tail;
            ret += "]";
            return ret;
            break;
        }
        case TTRecord: {
            const Record* casted = static_cast<Record*>(t.get());
            std::string ret = "{";
            for (auto it = casted->head.begin(); it != casted->head.end(); ++it) {
                if (it != casted->head.begin())
                    ret += ", ";
                ret += "'" + it->first + "': " + toString(it->second);
            }
            if (!casted->tail.empty())
                ret += "| $" + casted->tail;
            ret += "}";
            return ret;
            break;
        }
        case TTChoice: {
            const Choice* casted = static_cast<Choice*>(t.get());
            std::string ret = "(:";
            for (auto it = casted->head.begin(); it != casted->head.end(); ++it) {
                if (it != casted->head.begin())
                    ret += ", ";
                ret += it->first + ": " + toString(it->second);
            }
            if (!casted->tail.empty())
                ret += "| $" + casted->tail;
            ret += ":)";
            return ret;
            break;
        }
        case TTVar: {
            const Var* casted = static_cast<Var*>(t.get());
            return "$" + casted->value;
        }
        case TTEof: {
            return "???";
        }
    }
}

bool term::TermComparator::operator() (const std::pair<std::unique_ptr<term::Term>, std::unique_ptr<term::Term>> &p1,
                    const std::pair<std::unique_ptr<term::Term>, std::unique_ptr<term::Term>> &p2) const {
    return toString(p1.first) < toString(p2.first) || toString(p1.second) < toString(p2.second);
}

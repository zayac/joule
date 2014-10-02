#include "Interface.h"

namespace interface {

void Interface::addInputDeclaration(std::string variant_name, const message &msg) {
	if (input_variants.find(variant_name) == input_variants.end()) {
		input_variants[variant_name] = msg;
	} else {
		std::cerr << "conflict of input message types for variant " << variant_name << std::endl;
	}
}

void Interface::addOutputVolley(std::string variant_name, const std::pair<std::string, message> &v) {
	if (output_variants.find(variant_name) == output_variants.end()) {
		output_variants[variant_name] = volleys();
		output_variants[variant_name][v.first] = v.second;
		return;
	}

	volleys::const_iterator it = output_variants[variant_name].find(v.first);
	if (it != output_variants[variant_name].end() && it->second != v.second) {
		std::cerr << "conflict of message types for volley " << v.first <<" (functions that return 'message' cannot be overloaded)" << std::endl;
		return;
	}

	output_variants[variant_name][v.first] = v.second;
}

void Interface::printInputInterface() const {
	// iterate over input declarations
	std::cout << "(: ";
	for (std::map<std::string, message>::const_iterator vit = input_variants.begin(); vit != input_variants.end(); vit++) {
		if (vit != input_variants.begin())
			std::cout << ", ";
		std::cout << vit->first << ": ";
		printMessage(vit->second);
	}
	std::cout << " :)" << std::endl;
}

void Interface::printOutputInterface() const {
	// iterate over output declarations
	std::cout << "(: ";
	for (std::map<std::string, volleys>::const_iterator vit = output_variants.begin(); vit != output_variants.end(); vit++) {
		if (vit != output_variants.begin())
			std::cout << ", ";
		std::cout << vit->first << ": { ";
		for (volleys::const_iterator it = vit->second.begin(); it != vit->second.end(); it++) {
			if (it != vit->second.begin())
				std::cout << ", ";
			std::cout << it->first << ": "; 
			printMessage(it->second);
		}
		std::cout << " }";
	}
	std::cout << " :)" << std::endl;
}

void printMessage(const message &msg) {
	std::cout << "{ ";
	for (message::const_iterator mit = msg.begin(); mit != msg.end(); mit++) {
		if (mit != msg.begin())
			std::cout << ", ";
		std::cout << mit->first << ": " << mit->second;
	}
	std::cout << " }";
}

bool isValidType(const QualType& ty) {
    const QualType cty = ty.getCanonicalType();
    if ((cty->isPointerType() && cty.isConstQualified()) || cty->isReferenceType())
        return isValidType(cty->getPointeeType());
    else if (cty->isBuiltinType()) {
        return true;
    } else if (cty->isClassType() || cty->isStructureType()) {
        const RecordDecl *RD = cty->getAs<RecordType>()->getDecl();
        if (RD->isExternallyVisible())
            return true;
    }
    return false;
}

std::string getTypeAsString(const QualType& ty, bool quotation_marks) {
    std::string result = "";
    QualType cty(ty.getCanonicalType());
    /* remove volatile qualifier */
    if (cty.isLocalVolatileQualified())
        cty.removeLocalVolatile();
    if (cty->isPointerType() && cty.isConstQualified()) {
        result += getTypeAsString(cty->getPointeeType(), false) + "* const";
    } else if (cty->isReferenceType())
        result += getTypeAsString(cty->getPointeeType(), false) +  "&";
    else if (cty->isBuiltinType()) { /* builtin types are always canonical */
        result += std::string(cty.getCanonicalType().getAsString());
    } else if (cty->isClassType()) {
        std::string s = cty.getCanonicalType().getAsString();
        result += s.substr(std::string("class ").size());
    } else if (cty->isStructureType()) {
        std::string s = cty.getCanonicalType().getAsString();
        result += s.substr(std::string("struct ").size());
    }
    /* Invalid term. Need to call `isValidTerm' first. */
    if (result.empty())
        assert(0);
    if (quotation_marks)
        result = "\"" + result + "\"";
    return result;
}

}
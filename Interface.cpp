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
	std::cout << "(:";
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
        QualType deref = cty->getPointeeType();
        std::string s = ty.getAsString();
        //std::cout << s.substr(0, s.size() - 2) << " " << deref.getAsString() << std::endl;
        if (s.substr(0, s.size() - 2) == deref.getAsString())
            result += "self* const";
        else
            result += getTypeAsString(cty->getPointeeType(), false) +  "* const";
        //std::cout << ty.getAsString() << " " << cty->getPointeeType().getAsString() << std::endl;
        //result += getTypeAsString(cty->getPointeeType(), false) + "* const";
    } else if (cty->isReferenceType()) {
        QualType deref = cty->getPointeeType();
        std::string s = ty.getAsString();
        //std::cout << s.substr(0, s.size() - 2) << " " << deref.getAsString() << std::endl;
        if (s.substr(0, s.size() - 2) == deref.getAsString())
            result += "self&";
        else
            result += getTypeAsString(cty->getPointeeType(), false) +  "&";
    } else if (cty->isBuiltinType()) { /* builtin types are always canonical */
        result += std::string(cty.getCanonicalType().getAsString());
    } else if (cty->isClassType()) {
    	const CXXRecordDecl *record = cty->getAsCXXRecordDecl();
    	result += classDeclToString(getClassDecl(record));
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

class_repr::ClassDecl getClassDecl(const CXXRecordDecl *RD) {
	using namespace class_repr;
    std::vector<FieldDecl> field_decls;
    for (CXXRecordDecl::field_iterator fit = RD->field_begin(); fit != RD->field_end(); fit++) {
        FieldDecl field;
        if (fit->getAccess() == AS_public)
            field.access = Public_Access;
        else
            field.access = Private_Access;
        if (fit->getType().getCanonicalType().isVolatileQualified())
            field.is_volatile = true;
        field.name = fit->getDeclName().getAsString();
        if (isValidType(fit->getType())) {
            field.type = getTypeAsString(fit->getType());
        } else {
            std::cerr << "Type of field '" << field.name << "' that is a member of class '" << RD->getNameAsString() << "' does not have a valid term representation" << std::endl;
            exit(1);
        }
        field_decls.push_back(field);
    }
    std::vector<ClassDecl> class_decls;
    for (CXXRecordDecl::decl_iterator dit = RD->decls_begin(); dit != RD->decls_end(); dit++) {
        if (isa<CXXRecordDecl>(*dit)) {
            const CXXRecordDecl *record = dyn_cast<CXXRecordDecl>(*dit);
            if (record->isExternallyVisible() && !RD->isAbstract() && !record->isInjectedClassName()) {
                class_decls.push_back(getClassDecl(record));
            }
        }
    }
    std::vector<MethodDecl> method_decls;
    for(CXXRecordDecl::method_iterator mit = RD->method_begin(); mit != RD->method_end(); mit++) {
        MethodDecl method;
        if (mit->getAccess() == AS_public)
            method.access = Public_Access;
        else
            method.access = Private_Access;
        method.name = mit->getDeclName().getAsString();
        // we don't want to add destructor to a term description
        if (method.name == "~" + RD->getNameAsString())
            continue;
        method.is_const = mit->isConst();
        if (isValidType(mit->getReturnType())) {
            method.return_term = getTypeAsString(mit->getReturnType());
        } else {
            std::cerr << "Return type of method '" << method.name << "' that is a member of class '" << RD->getNameAsString() << "' does not have a valid term representation" << std::endl;
            exit(1);
        }

        bool all_parameters_valid = true;
        std::vector<std::string> params_terms;
        for (FunctionDecl::param_const_iterator pit = mit->param_begin(); pit != mit->param_end(); pit++) {
            const ParmVarDecl par = **pit;
            if (isValidType(par.getOriginalType())) {
                params_terms.push_back(getTypeAsString(par.getOriginalType(), false));
            } else {
                all_parameters_valid = false;
                break;
            }
        }
        if (all_parameters_valid) {
            method.params_terms = params_terms;
        } else {
            std::cerr << "Some parameters of method '" << method.name << "' that is a member of class '" << RD->getNameAsString() << "' does not have a valid term representation" << std::endl;
            exit(1);
        }
        method_decls.push_back(method);
    }
   	ClassDecl c;
    c.name = RD->getNameAsString();
    c.classes = class_decls;
    c.methods = method_decls;
    c.fields = field_decls;
    return c;
}

std::string classDeclToString(const class_repr::ClassDecl& c) {
	using namespace class_repr;

	std::string result = "";

    if (c.methods.empty() && c.fields.empty()) {
        result += "nil";
    } else {
        result += "{ ";
        for (unsigned i = 0; i < c.classes.size(); i++) {
            ClassDecl cl = c.classes[i];
            result += cl.name + ": " + classDeclToString(cl) + ", ";
        }
        for (unsigned i = 0; i < c.fields.size(); i++) {
            FieldDecl field = c.fields[i];
            result += field.name + ": (";
            if (field.access == Public_Access)
                result += "public ";
            else
                result += "private ";
            if (field.is_volatile)
                result += "volatile ";
            result += field.type + ")";
            if (i != c.fields.size() - 1 || !c.methods.empty()) {
                result += ", ";
            }
        }
        for (unsigned i = 0; i < c.methods.size(); i++) {
            MethodDecl method = c.methods[i];
            result += "\"" + method.name + "(";
            for (unsigned j = 0; j < method.params_terms.size(); j++) {
                result += method.params_terms[j];
                if (j < method.params_terms.size() - 1)
                    result += ", ";
            }
            result += ")";
            if (method.is_const) {
                result += " const";
            }
            result += "\": (";
            result += ((method.access == Public_Access) ? "public " : "private ") + method.return_term + ")";
            if (i != c.methods.size() - 1) {
                result += ", ";
            }
        }
        result += "}";
    }
    return result;
}

}

#ifndef CONFIRMDRESULT_H
#define CONFIRMDRESULT_H


#include "result.h"


class ConfirmdResult : public Result
{
public:
	static const char *TYPE;

private:
	int m_confirmedHeader;

public:
	ConfirmdResult(int confirmedHeader);

	int getConfirmedHeader() const;

	const char *getType() const;
};

#endif // CONFIRMDRESULT_H
